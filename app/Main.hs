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
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                )
import           Data.Function                  ( (&) )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(Down) )
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
import qualified Graphics.Vulkan.Ext.VK_KHR_surface
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
  SDL.vkLoadLibrary Nothing

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

  surface :: SDL.VkSurfaceKHR <-
    logMsg "Creating SDL surface"
      *> SDL.vkCreateSurface window (Foreign.castPtr vulkanInstance)

  assertSurfacePresentable physicalDevice queueFamilyIndex surface

  (format :: Vulkan.VkFormat, colorSpace :: Vulkan.VkColorSpaceKHR) <-
    logMsg "Finding correct swapchain format and color space"
      *> determineSwapchainFormat physicalDevice surface

  (swapchain :: Vulkan.VkSwapchainKHR, extent :: Vulkan.VkExtent2D) <-
    logMsg "Creating swapchain"
      *> createSwapchain physicalDevice device surface format colorSpace

  images :: [Vulkan.VkImage] <-
    logMsg "Getting swapchain images" *> getSwapchainImages device swapchain

  depthFormat :: Vulkan.VkFormat <- logMsg "Finding optimal depth format"
    *> findOptimalDepthFormat physicalDevice

  renderPass :: Vulkan.VkRenderPass <-
    logMsg "Creating a render pass"
      *> createRenderPass device depthFormat format

  SDL.showWindow window

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.vkUnloadLibrary
  SDL.quit


-- from zero to quake 3

createRenderPass
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> Vulkan.VkFormat
  -> m Vulkan.VkRenderPass
createRenderPass dev depthFormat imageFormat = do
  let
    colorAttachmentDescription = Vulkan.createVk
      (  Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"format" imageFormat
      &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
      &* Vulkan.set @"loadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
      &* Vulkan.set @"storeOp" Vulkan.VK_ATTACHMENT_STORE_OP_STORE
      &* Vulkan.set @"stencilLoadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
      &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
      &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      &* Vulkan.set @"finalLayout" Vulkan.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
      )

    depthAttachmentDescription = Vulkan.createVk
      (  Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"format" depthFormat
      &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
      &* Vulkan.set @"loadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_CLEAR
      &* Vulkan.set @"storeOp" Vulkan.VK_ATTACHMENT_STORE_OP_STORE
      &* Vulkan.set @"stencilLoadOp" Vulkan.VK_ATTACHMENT_LOAD_OP_DONT_CARE
      &* Vulkan.set @"stencilStoreOp" Vulkan.VK_ATTACHMENT_STORE_OP_DONT_CARE
      &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      &* Vulkan.set @"finalLayout"
           Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      )

    colorAttachmentReference = Vulkan.createVk
      (  Vulkan.set @"attachment" 0
      &* Vulkan.set @"layout" Vulkan.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      )

    depthAttachmentReference = Vulkan.createVk
      (  Vulkan.set @"attachment" 1
      &* Vulkan.set @"layout"
           Vulkan.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
      )

    subpass = Vulkan.createVk
      (  Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"pipelineBindPoint"
           Vulkan.VK_PIPELINE_BIND_POINT_GRAPHICS
      &* Vulkan.set @"colorAttachmentCount" 1
      &* Vulkan.setListRef @"pColorAttachments" [colorAttachmentReference]
      &* Vulkan.set @"inputAttachmentCount" 0
      &* Vulkan.set @"pInputAttachments" Vulkan.vkNullPtr
      &* Vulkan.set @"pResolveAttachments" Vulkan.vkNullPtr
      &* Vulkan.setVkRef @"pDepthStencilAttachment" depthAttachmentReference
      &* Vulkan.set @"preserveAttachmentCount" 0
      &* Vulkan.set @"pPreserveAttachments" Vulkan.vkNullPtr
      )

    dependency1 = Vulkan.createVk
      (  Vulkan.set @"srcSubpass" Vulkan.VK_SUBPASS_EXTERNAL
      &* Vulkan.set @"dstSubpass" 0
      &* Vulkan.set @"srcStageMask"
           Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* Vulkan.set @"srcAccessMask" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"dstStageMask"
           Vulkan.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      &* Vulkan.set @"dstAccessMask"
           (   Vulkan.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
           .|. Vulkan.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
           )
      )

    createInfo = Vulkan.createVk
      (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
      &* Vulkan.set @"pNext" Vulkan.vkNullPtr
      &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"attachmentCount" 2
      &* Vulkan.setListRef @"pAttachments"
           [colorAttachmentDescription, depthAttachmentDescription]
      &* Vulkan.set @"subpassCount" 1
      &* Vulkan.setListRef @"pSubpasses" [subpass]
      &* Vulkan.set @"dependencyCount" 1
      &* Vulkan.setListRef @"pDependencies" [dependency1]
      )

  managedVulkanResource
    (Vulkan.vkCreateRenderPass dev (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroyRenderPass dev)

findOptimalDepthFormat
  :: forall  m . MonadIO m => Vulkan.VkPhysicalDevice -> m Vulkan.VkFormat
findOptimalDepthFormat physicalDevice = findFirstSupported
  [ Vulkan.VK_FORMAT_D32_SFLOAT
  , Vulkan.VK_FORMAT_D32_SFLOAT_S8_UINT
  , Vulkan.VK_FORMAT_D24_UNORM_S8_UINT
  ]
 where
  findFirstSupported :: [Vulkan.VkFormat] -> m Vulkan.VkFormat
  findFirstSupported fmtList = case fmtList of
    []       -> fail "Could not find a valid depth format"
    (x : xs) -> do
      properties <- allocaAndPeek
        (Vulkan.vkGetPhysicalDeviceFormatProperties physicalDevice x)
      if supportsDepthAttachment properties
        then pure x
        else findFirstSupported xs

  supportsDepthAttachment :: Vulkan.VkFormatProperties -> Bool
  supportsDepthAttachment props = testBitmask
    (Vulkan.getField @"optimalTilingFeatures" props)
    Vulkan.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT

  testBitmask mask featureFlag = mask .&. featureFlag == featureFlag


getSwapchainImages
  :: MonadIO m => Vulkan.VkDevice -> Vulkan.VkSwapchainKHR -> m [Vulkan.VkImage]
getSwapchainImages device swapchain = liftIO $ fetchAll
  (\imageCountPtr imagesPtr ->
    Vulkan.vkGetSwapchainImagesKHR device swapchain imageCountPtr imagesPtr
      >>= throwVkResult
  )

createSwapchain
  :: (MonadIO m, MonadManaged m)
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> SDL.VkSurfaceKHR
  -> Vulkan.VkFormat
  -> Vulkan.VkColorSpaceKHR
  -> m (Vulkan.VkSwapchainKHR, Vulkan.VkExtent2D)
createSwapchain physicalDevice device surface format colorSpace = do
  surfaceCapabilities <- liftIO $ allocaAndPeek
    (   Vulkan.vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice
                                                         (Vulkan.VkPtr surface)
    >=> throwVkResult
    )

  let
    minImageCount    = Vulkan.getField @"minImageCount" surfaceCapabilities
    currentExtent    = Vulkan.getField @"currentExtent" surfaceCapabilities
    currentTransform = Vulkan.getField @"currentTransform" surfaceCapabilities

    swapchainCreateInfo :: Vulkan.VkSwapchainCreateInfoKHR = Vulkan.createVk
      (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.set @"surface" (Vulkan.VkPtr surface)
      &* Vulkan.set @"minImageCount" minImageCount
      &* Vulkan.set @"imageFormat" format
      &* Vulkan.set @"imageColorSpace" colorSpace
      &* Vulkan.set @"imageExtent" currentExtent
      &* Vulkan.set @"imageArrayLayers" 1
      &* Vulkan.set @"imageUsage" Vulkan.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      &* Vulkan.set @"imageSharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
      &* Vulkan.set @"queueFamilyIndexCount" 0
      &* Vulkan.set @"pQueueFamilyIndices" Vulkan.VK_NULL
      &* Vulkan.set @"preTransform" currentTransform
      &* Vulkan.set @"compositeAlpha" Vulkan.VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
      &* Vulkan.set @"presentMode" Vulkan.VK_PRESENT_MODE_FIFO_KHR
      &* Vulkan.set @"clipped" Vulkan.VK_TRUE
      &* Vulkan.set @"oldSwapchain" Vulkan.VK_NULL_HANDLE
      )

  swapchain <- managedVulkanResource
    (Vulkan.vkCreateSwapchainKHR device (Vulkan.unsafePtr swapchainCreateInfo))
    (Vulkan.vkDestroySwapchainKHR device)

  pure (swapchain, currentExtent)

determineSwapchainFormat
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> SDL.VkSurfaceKHR
  -> m (Vulkan.VkFormat, Vulkan.VkColorSpaceKHR)
determineSwapchainFormat physicalDevice surface = liftIO $ do
  surfaceFormats :: [Vulkan.VkSurfaceFormatKHR] <- fetchAll
    (\surfaceFormatCountPtr surfaceFormatsPtr ->
      Vulkan.vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice
                                                  (Vulkan.VkPtr surface)
                                                  surfaceFormatCountPtr
                                                  surfaceFormatsPtr
        >>= throwVkResult
    )

  let score
        :: Vulkan.VkSurfaceFormatKHR
        -> (Int, Vulkan.VkFormat, Vulkan.VkColorSpaceKHR)
      score surfaceFormat = (intScore, format, colorSpace)
       where
        intScore = sum
          [ if format == Vulkan.VK_FORMAT_B8G8R8A8_UNORM then 1 else 0
          , if colorSpace == Vulkan.VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
            then 1
            else 0
          ]
        format     = Vulkan.getField @"format" surfaceFormat
        colorSpace = Vulkan.getField @"colorSpace" surfaceFormat

      scoredFormats :: [(Vulkan.VkFormat, Vulkan.VkColorSpaceKHR)] =
        fmap score surfaceFormats
          & sortOn (\(intScore, _, _) -> Down intScore)
          & fmap (\(_, format, colorSpace) -> (format, colorSpace))

  case scoredFormats of
    []    -> fail "No formats found"
    x : _ -> pure x

assertSurfacePresentable
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.Word32
  -> SDL.VkSurfaceKHR
  -> m ()
assertSurfacePresentable physicalDevice queueFamilyIndex surface = liftIO $ do
  bool <- allocaAndPeek
    (   Vulkan.vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice
                                                    queueFamilyIndex
                                                    (Vulkan.VkPtr surface)
    >=> throwVkResult
    )
  unless (bool == Vulkan.VK_TRUE) (fail "Unsupported surface")

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
