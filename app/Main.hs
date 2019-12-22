{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Control.Exception
import           Control.Monad                  ( unless
                                                , (>=>)
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Managed
import           Control.Monad.Managed          ( MonadManaged )
import           Data.Bits                      ( (.&.) )
import           Data.Function                  ( (&) )
import           Data.Traversable               ( for )
import qualified Foreign
import qualified Foreign.C
import qualified Foreign.Marshal
import qualified Graphics.Vulkan               as Vulkan
import qualified Graphics.Vulkan.Core_1_0      as Vulkan
import qualified Graphics.Vulkan.Marshal.Create
                                               as Vulkan
import           Graphics.Vulkan.Marshal.Create ( (&*) )
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain
                                               as Vulkan
import           Linear                         ( V2(V2) )
import qualified SDL
import           SDL                            ( ($=) )
import qualified SDL.Internal.Exception
import qualified SDL.Raw
import qualified SDL.Video.Vulkan              as SDL

--
--
main :: IO ()
main = Control.Monad.Managed.runManaged $ do
  -- set up SDL
  logMsg "Enabling verbose SDL logging"
  enableSDLLogging
  logMsg "Initializing SDL"
  SDL.initialize [SDL.InitVideo]

  window           <- createWindow

  neededExtensions <- getNeededVulkanExtensions window
  logMsg $ "Needed Vulkan extensions: " <> show neededExtensions

  vulkanInstance :: Vulkan.VkInstance <- logMsg "Creating Vulkan instance"
    *> createVulkanInstance neededExtensions

  physicalDevice :: Vulkan.VkPhysicalDevice <- logMsg "Creating physical device"
    *> createPhysicalDevice vulkanInstance

  queueFamilyIndex :: Vulkan.Word32 <- logMsg "Finding suitable queue family"
    *> findQueueFamilyIndex physicalDevice

  device :: Vulkan.VkDevice <-
    logMsg "Creating logical device"
      *> createDevice physicalDevice queueFamilyIndex

  SDL.showWindow window

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        unless quit loop

  loop

  SDL.quit


-- from zero to quake 3

createDevice
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.Word32
  -> m Vulkan.VkDevice
createDevice physicalDevice queueFamilyIndex = do
  let queueCreateInfo :: Vulkan.VkDeviceQueueCreateInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"queueFamilyIndex" queueFamilyIndex
        &* Vulkan.set @"queueCount" 1
        &* Vulkan.setListRef @"pQueuePriorities" [1.0 :: Float]
        )
      createInfo :: Vulkan.VkDeviceCreateInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" (toEnum 0)
        &* Vulkan.set @"queueCreateInfoCount" 1
        &* Vulkan.setListRef @"pQueueCreateInfos" [queueCreateInfo]
        &* Vulkan.set @"enabledLayerCount" 0
        &* Vulkan.set @"ppEnabledLayerNames" Vulkan.VK_NULL
        &* Vulkan.set @"enabledExtensionCount" 1
        &* Vulkan.setListRef @"ppEnabledExtensionNames"
             [Vulkan.VK_KHR_SWAPCHAIN_EXTENSION_NAME]
        &* Vulkan.set @"pEnabledFeatures" Vulkan.VK_NULL
        )
  managedVulkanResource
    (Vulkan.vkCreateDevice physicalDevice (Vulkan.unsafePtr createInfo))
    Vulkan.vkDestroyDevice

findQueueFamilyIndex :: MonadIO m => Vulkan.VkPhysicalDevice -> m Vulkan.Word32
findQueueFamilyIndex physicalDevice = liftIO $ do
  queueFamilies :: [Vulkan.VkQueueFamilyProperties] <- fetchAll
    (\nQueueFamiliesPtr queueFamiliesPtr ->
      Vulkan.vkGetPhysicalDeviceQueueFamilyProperties physicalDevice
                                                      nQueueFamiliesPtr
                                                      queueFamiliesPtr
    )

  let isCapable :: Vulkan.VkQueueFamilyProperties -> Bool
      isCapable p =
        (Vulkan.getField @"queueFlags" p)
          .&. Vulkan.VK_QUEUE_GRAPHICS_BIT
          ==  Vulkan.VK_QUEUE_GRAPHICS_BIT

      capableFamilyIndices :: [Vulkan.Word32] =
        zip [0 ..] queueFamilies & filter (isCapable . snd) & fmap fst

  case capableFamilyIndices of
    []      -> fail "No queue family has sufficient (graphics) capabilities"
    (i : _) -> pure i

createPhysicalDevice
  :: MonadIO m => Vulkan.VkInstance -> m Vulkan.VkPhysicalDevice
createPhysicalDevice vk = liftIO $ do
  physicalDevices :: [Vulkan.VkPhysicalDevice] <- fetchAll
    (\nPtr ptr ->
      Vulkan.vkEnumeratePhysicalDevices vk nPtr ptr >>= throwVkResult
    )
  typedDevices :: [(Vulkan.VkPhysicalDevice, Vulkan.VkPhysicalDeviceType)] <-
    for physicalDevices $ \physicalDevice -> do
      properties <- allocaAndPeek
        (Vulkan.vkGetPhysicalDeviceProperties physicalDevice)
      let deviceType = Vulkan.getField @"deviceType" properties
      pure (physicalDevice, deviceType)
  let isSuitableDevice
        :: (Vulkan.VkPhysicalDevice, Vulkan.VkPhysicalDeviceType) -> Bool
      isSuitableDevice (_, deviceType) =
        deviceType
          `elem` [ Vulkan.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
                 , Vulkan.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
                 ]
  case filter isSuitableDevice typedDevices of
    []         -> fail "Could not find a suitable physical device"
    (d, _) : _ -> pure d

logMsg :: MonadIO m => String -> m ()
logMsg = liftIO . putStrLn

enableSDLLogging :: MonadIO m => m ()
enableSDLLogging = SDL.Raw.logSetAllPriority SDL.Raw.SDL_LOG_PRIORITY_VERBOSE

getNeededVulkanExtensions :: MonadIO m => SDL.Window -> m [String]
getNeededVulkanExtensions window = do
  extRaw <- SDL.vkGetInstanceExtensions window
  traverse (liftIO . Foreign.C.peekCString) extRaw

createWindow :: MonadManaged m => m SDL.Window
createWindow = manageBracket create destroy
 where
  create = SDL.createWindow
    "Vulkan"
    SDL.defaultWindow { SDL.windowInitialSize     = V2 640 480
                      , SDL.windowGraphicsContext = SDL.VulkanContext
                      }
  destroy = SDL.destroyWindow

createVulkanInstance :: MonadManaged m => [String] -> m Vulkan.VkInstance
createVulkanInstance extensions = managedVulkanResource
  (Vulkan.vkCreateInstance (Vulkan.unsafePtr createInfo))
  Vulkan.vkDestroyInstance
 where
  createInfo :: Vulkan.VkInstanceCreateInfo
  createInfo = Vulkan.createVk
    (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
    &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
    &* Vulkan.set @"flags" (toEnum 0)
    &* Vulkan.set @"pApplicationInfo" Vulkan.VK_NULL_HANDLE
    &* Vulkan.set @"enabledLayerCount" 1
    &* Vulkan.setStrListRef @"ppEnabledLayerNames"
         ["VK_LAYER_LUNARG_standard_validation"]
    &* Vulkan.set @"enabledExtensionCount" nExtensions
    &* Vulkan.setStrListRef @"ppEnabledExtensionNames" extensions
    )
  nExtensions = fromIntegral (length extensions)

managedVulkanResource
  :: (MonadManaged m, Foreign.Storable x, Vulkan.VulkanPtr ptr)
  => (ptr a -> Vulkan.Ptr x -> IO Vulkan.VkResult)
  -> (x -> ptr a -> IO ())
  -> m x
managedVulkanResource create destroy = manageBracket
  (allocaAndPeek (create Vulkan.vkNullPtr >=> throwVkResult))
  (\a -> destroy a Vulkan.vkNullPtr)

throwVkResult :: MonadIO m => Vulkan.VkResult -> m ()
throwVkResult Vulkan.VK_SUCCESS = return ()
throwVkResult res               = fail (show res)

managed :: MonadManaged m => (forall r . (a -> IO r) -> IO r) -> m a
managed f = Control.Monad.Managed.using (Control.Monad.Managed.managed f)

manageBracket :: MonadManaged m => IO a -> (a -> IO b) -> m a
manageBracket create destroy =
  managed (Control.Exception.bracket create destroy)

-- | Allocates an `a`, performs an action with a pointer to it, and returns the
--   `a`.
allocaAndPeek
  :: (Foreign.Storable a, MonadIO m)
  => (Foreign.Ptr a -> IO ())  -- ^ Action to perform with the `Ptr a` value.
  -> m a                       -- ^ The `a` value in monad `m`.
allocaAndPeek f =
  liftIO $ Foreign.Marshal.alloca (\ptr -> f ptr *> Foreign.peek ptr)

fetchAll
  :: (Foreign.Storable a, Foreign.Storable b, Integral b)
  => (Foreign.Ptr b -> Foreign.Ptr a -> IO ())
  -> IO [a]
fetchAll f = do
  Foreign.Marshal.alloca $ \nPtr -> do
    f nPtr Foreign.nullPtr
    n <- fromIntegral <$> Foreign.peek nPtr
    allocaAndPeekArray n (f nPtr)

allocaAndPeekArray
  :: Foreign.Storable a => Int -> (Foreign.Ptr a -> IO ()) -> IO [a]
allocaAndPeekArray n f = Foreign.Marshal.allocaArray
  n
  (\ptr -> f ptr *> Foreign.Marshal.peekArray n ptr)
