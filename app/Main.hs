{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import qualified Control.Exception
import           Control.Monad                  ( unless
                                                , (>=>)
                                                , guard
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Managed
import           Control.Monad.Managed          ( MonadManaged )
import           Data.Bits                      ( (.&.)
                                                , (.|.)
                                                , testBit
                                                )
import qualified Data.ByteString
import           Data.Foldable                  ( for_ )
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

  framebuffers :: [Vulkan.VkFramebuffer] <- do
    logMsg "Creating frame buffers"
    for images $ \image -> do
      imageView :: Vulkan.VkImageView <-
        logMsg "Creating color image view"
          *> createImageView device
                             image
                             format
                             Vulkan.VK_IMAGE_ASPECT_COLOR_BIT
      depthImage :: Vulkan.VkImage <-
        logMsg "Creating depth image"
          *> createDepthImage physicalDevice device depthFormat extent
      depthImageView :: Vulkan.VkImageView <-
        logMsg "Creating depth image view"
          *> createImageView device
                             depthImage
                             depthFormat
                             Vulkan.VK_IMAGE_ASPECT_DEPTH_BIT
      createFramebuffer device renderPass imageView depthImageView extent

  commandPool :: Vulkan.VkCommandPool <-
    logMsg "Creating command pool" *> createCommandPool device queueFamilyIndex

  queue :: Vulkan.VkQueue <-
    logMsg "Getting command queue" *> getQueue device queueFamilyIndex

  nextImageSem :: Vulkan.VkSemaphore                  <- createSemaphore device
  submittedSem :: Vulkan.VkSemaphore                  <- createSemaphore device

  descriptorSetLayout :: Vulkan.VkDescriptorSetLayout <-
    createDescriptorSetLayout device
  descriptorPool :: Vulkan.VkDescriptorPool <- createDescriptorPool device
  descriptorSet :: Vulkan.VkDescriptorSet   <- allocateDescriptorSet
    device
    descriptorPool
    descriptorSetLayout

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

createPipeline
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkExtent2D
  -> Vulkan.VkDescriptorSetLayout
  -> m (Vulkan.VkPipeline, Vulkan.VkPipelineLayout)
createPipeline device renderPass extent layout0 = do
  pipelineLayout <-
    let
      pipelineLayoutCreateInfo = Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"setLayoutCount" 1
        &* Vulkan.setListRef @"pSetLayouts" [layout0]
        &* Vulkan.set @"pPushConstantRanges" Vulkan.VK_NULL
        )
    in  managedVulkanResource
          (Vulkan.vkCreatePipelineLayout
            device
            (Vulkan.unsafePtr pipelineLayoutCreateInfo)
          )
          (Vulkan.vkDestroyPipelineLayout device)

  -- TODO Remove hard coding
  vertexShader   <- loadShader device "/home/ollie/work/zero-to-quake3/vert.spv"

  fragmentShader <- loadShader device "/home/ollie/work/zero-to-quake3/frag.spv"

  let
    rasterizationCreateInfo :: Vulkan.VkPipelineRasterizationStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"depthClampEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"rasterizerDiscardEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"polygonMode" Vulkan.VK_POLYGON_MODE_FILL
        &* Vulkan.set @"lineWidth" 1
        &* Vulkan.set @"depthBiasEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"depthBiasSlopeFactor" 0
        &* Vulkan.set @"depthBiasClamp" 0
        &* Vulkan.set @"depthBiasConstantFactor" 0
        &* Vulkan.set @"frontFace" Vulkan.VK_FRONT_FACE_CLOCKWISE
        &* Vulkan.set @"cullMode" Vulkan.VK_CULL_MODE_BACK_BIT
        )

    vertexShaderStage :: Vulkan.VkPipelineShaderStageCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setStrRef @"pName" "main"
        &* Vulkan.set @"module" vertexShader
        &* Vulkan.set @"stage" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
        )

    fragmentShaderStage :: Vulkan.VkPipelineShaderStageCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.setStrRef @"pName" "main"
        &* Vulkan.set @"module" fragmentShader
        &* Vulkan.set @"stage" Vulkan.VK_SHADER_STAGE_FRAGMENT_BIT
        )

  {-
    vertexBindingDescription =
      Vulkan.createVk
        (  Vulkan.set @"binding" 0
        &* Vulkan.set @"stride" 8 -- TODO
        &* Vulkan.set @"inputRate" Vulkan.VK_VERTEX_INPUT_RATE_VERTEX
        )

    vertexInputAttributeDescriptions =
      VertexFormat.attributeDescriptions 0 vertexFormat

    vertexInputState =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"vertexBindingDescriptionCount" 1
        &* Vulkan.setListRef @"pVertexBindingDescriptions" [ vertexBindingDescription ]
        &* Vulkan.set @"vertexAttributeDescriptionCount" ( fromIntegral ( length vertexInputAttributeDescriptions ) )
        &* Vulkan.setListRef @"pVertexAttributeDescriptions" vertexInputAttributeDescriptions
        )
    -}

    assemblyStateCreateInfo :: Vulkan.VkPipelineInputAssemblyStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"topology" Vulkan.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
        &* Vulkan.set @"primitiveRestartEnable" Vulkan.VK_FALSE
        )

    viewport :: Vulkan.VkViewport = Vulkan.createVk
      (  Vulkan.set @"x" 0
      &* Vulkan.set @"y" 0
      &* Vulkan.set @"width" (fromIntegral (Vulkan.getField @"width" extent))
      &* Vulkan.set @"height"
           (fromIntegral (Vulkan.getField @"height" extent))
      &* Vulkan.set @"minDepth" 0
      &* Vulkan.set @"maxDepth" 1
      )

    scissor :: Vulkan.VkRect2D =
      let offset = Vulkan.createVk (Vulkan.set @"x" 0 &* Vulkan.set @"y" 0)
      in  Vulkan.createVk
            (Vulkan.set @"offset" offset &* Vulkan.set @"extent" extent)

    viewportState :: Vulkan.VkPipelineViewportStateCreateInfo = Vulkan.createVk
      (  Vulkan.set @"sType"
          Vulkan.VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"viewportCount" 1
      &* Vulkan.set @"scissorCount" 1
      &* Vulkan.setListRef @"pViewports" [viewport]
      &* Vulkan.setListRef @"pScissors" [scissor]
      )

    multisampleState :: Vulkan.VkPipelineMultisampleStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
        &* Vulkan.set @"minSampleShading" 1
        &* Vulkan.set @"rasterizationSamples" Vulkan.VK_SAMPLE_COUNT_1_BIT
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    attachmentState :: Vulkan.VkPipelineColorBlendAttachmentState =
      Vulkan.createVk
        (  Vulkan.set @"blendEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"alphaBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstColorBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorBlendOp" Vulkan.VK_BLEND_OP_ADD
        &* Vulkan.set @"srcAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ONE
        &* Vulkan.set @"dstAlphaBlendFactor" Vulkan.VK_BLEND_FACTOR_ZERO
        &* Vulkan.set @"colorWriteMask"
             (   Vulkan.VK_COLOR_COMPONENT_R_BIT
             .|. Vulkan.VK_COLOR_COMPONENT_G_BIT
             .|. Vulkan.VK_COLOR_COMPONENT_B_BIT
             .|. Vulkan.VK_COLOR_COMPONENT_A_BIT
             )
        )

    colorBlendState :: Vulkan.VkPipelineColorBlendStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
        &* Vulkan.setAt @"blendConstants" @0 0
        &* Vulkan.setAt @"blendConstants" @1 0
        &* Vulkan.setAt @"blendConstants" @2 0
        &* Vulkan.setAt @"blendConstants" @3 0
        &* Vulkan.set @"attachmentCount" 1
        &* Vulkan.setListRef @"pAttachments" [attachmentState]
        &* Vulkan.set @"logicOp" Vulkan.VK_LOGIC_OP_COPY
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        )

    nullStencilOp :: Vulkan.VkStencilOpState = Vulkan.createVk
      (  Vulkan.set @"reference" 0
      &* Vulkan.set @"writeMask" 0
      &* Vulkan.set @"compareMask" 0
      &* Vulkan.set @"compareOp" Vulkan.VK_COMPARE_OP_EQUAL
      &* Vulkan.set @"depthFailOp" Vulkan.VK_STENCIL_OP_KEEP
      &* Vulkan.set @"passOp" Vulkan.VK_STENCIL_OP_KEEP
      &* Vulkan.set @"failOp" Vulkan.VK_STENCIL_OP_KEEP
      )

    depthStencilState :: Vulkan.VkPipelineDepthStencilStateCreateInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType"
            Vulkan.VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"depthTestEnable" Vulkan.VK_TRUE
        &* Vulkan.set @"depthWriteEnable" Vulkan.VK_TRUE
        &* Vulkan.set @"depthCompareOp" Vulkan.VK_COMPARE_OP_LESS_OR_EQUAL
        &* Vulkan.set @"maxDepthBounds" 1
        &* Vulkan.set @"minDepthBounds" 0
        &* Vulkan.set @"stencilTestEnable" Vulkan.VK_FALSE
        &* Vulkan.set @"front" nullStencilOp
        &* Vulkan.set @"back" nullStencilOp
        )

    {-
    createInfo =
      Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"stageCount" 2
        &* Vulkan.setListRef @"pStages" [ vertexShaderStage, fragmentShaderStage ]
        &* Vulkan.setVkRef @"pVertexInputState" vertexInputState
        &* Vulkan.set @"basePipelineIndex" 0
        &* Vulkan.set @"subpass" 0
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"layout" pipelineLayout
        &* Vulkan.setVkRef @"pRasterizationState" rasterizationCreateInfo
        &* Vulkan.setVkRef @"pInputAssemblyState" assemblyStateCreateInfo
        &* Vulkan.setVkRef @"pViewportState" viewportState
        &* Vulkan.setVkRef @"pMultisampleState" multisampleState
        &* Vulkan.setVkRef @"pColorBlendState" colorBlendState
        &* Vulkan.setVkRef @"pDepthStencilState" depthStencilState
        )

  pipeline <-
    managedVulkanResource
      ( Vulkan.vkCreateGraphicsPipelines
          device
          Vulkan.vkNullPtr
          1
          ( Vulkan.unsafePtr createInfo )
      )
      ( Vulkan.vkDestroyPipeline device )

  return ( pipeline, pipelineLayout )
  --}
  undefined

loadShader
  :: MonadManaged m => Vulkan.VkDevice -> FilePath -> m Vulkan.VkShaderModule
loadShader device srcFile = do
  bytes <- liftIO (Data.ByteString.readFile srcFile)

  managedVulkanResource
    (\a b -> Data.ByteString.useAsCStringLen bytes $ \(bytesPtr, len) ->
      let
        createInfo = Vulkan.createVk
          (  Vulkan.set @"sType"
              Vulkan.VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
          &* Vulkan.set @"pNext" Vulkan.VK_NULL
          &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
          &* Vulkan.set @"pCode" (Foreign.castPtr bytesPtr)
          &* Vulkan.set @"codeSize" (fromIntegral len)
          )
      in  Vulkan.vkCreateShaderModule device (Vulkan.unsafePtr createInfo) a b
    )
    (Vulkan.vkDestroyShaderModule device)

allocateDescriptorSet
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkDescriptorPool
  -> Vulkan.VkDescriptorSetLayout
  -> m Vulkan.VkDescriptorSet
allocateDescriptorSet dev descriptorPool layout0 = do
  let
    allocateInfo = Vulkan.createVk
      (Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.set @"descriptorPool" descriptorPool
      &* Vulkan.set @"descriptorSetCount" 1
      &* Vulkan.setListRef @"pSetLayouts" [layout0]
      )

  manageBracket
    (allocaAndPeek
      (   Vulkan.vkAllocateDescriptorSets dev (Vulkan.unsafePtr allocateInfo)
      >=> throwVkResult
      )
    )
    (\a -> Foreign.Marshal.withArray
      [a]
      (Vulkan.vkFreeDescriptorSets dev descriptorPool 1)
    )

createDescriptorPool
  :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkDescriptorPool
createDescriptorPool device =
  let
    poolSize0 = Vulkan.createVk
      (  Vulkan.set @"type" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
      &* Vulkan.set @"descriptorCount" 1
      )

    createInfo = Vulkan.createVk
      (Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"poolSizeCount" 1
      &* Vulkan.setListRef @"pPoolSizes" [poolSize0]
      &* Vulkan.set @"maxSets" 1
      )
  in
    managedVulkanResource
      (Vulkan.vkCreateDescriptorPool device (Vulkan.unsafePtr createInfo))
      (Vulkan.vkDestroyDescriptorPool device)

createDescriptorSetLayout
  :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkDescriptorSetLayout
createDescriptorSetLayout device = do
  let
    binding = Vulkan.createVk
      (  Vulkan.set @"binding" 0
      &* Vulkan.set @"descriptorType" Vulkan.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
      &* Vulkan.set @"descriptorCount" 1
      &* Vulkan.set @"stageFlags" Vulkan.VK_SHADER_STAGE_VERTEX_BIT
      &* Vulkan.set @"pImmutableSamplers" Vulkan.VK_NULL
      )

    createInfo = Vulkan.createVk
      (  Vulkan.set @"sType"
          Vulkan.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"bindingCount" 1
      &* Vulkan.setListRef @"pBindings" [binding]
      )

  managedVulkanResource
    (Vulkan.vkCreateDescriptorSetLayout device (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroyDescriptorSetLayout device)


createSemaphore :: MonadManaged m => Vulkan.VkDevice -> m Vulkan.VkSemaphore
createSemaphore device = do
  let createInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL_HANDLE
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        )

  managedVulkanResource
    (Vulkan.vkCreateSemaphore device (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroySemaphore device)

getQueue :: MonadIO m => Vulkan.VkDevice -> Vulkan.Word32 -> m Vulkan.VkQueue
getQueue device queueFamilyIndex =
  liftIO $ allocaAndPeek (Vulkan.vkGetDeviceQueue device queueFamilyIndex 0)

createCommandPool
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.Word32
  -> m Vulkan.VkCommandPool
createCommandPool dev queueFamilyIndex = do
  let createInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"queueFamilyIndex" queueFamilyIndex
        )

  managedVulkanResource
    (Vulkan.vkCreateCommandPool dev (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroyCommandPool dev)

createFramebuffer
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkRenderPass
  -> Vulkan.VkImageView
  -> Vulkan.VkImageView
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkFramebuffer
createFramebuffer dev renderPass colorImageView depthView extent = do
  let createInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.vkNullPtr
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"renderPass" renderPass
        &* Vulkan.set @"attachmentCount" 2
        &* Vulkan.setListRef @"pAttachments" [colorImageView, depthView]
        &* Vulkan.set @"width" (Vulkan.getField @"width" extent)
        &* Vulkan.set @"height" (Vulkan.getField @"height" extent)
        &* Vulkan.set @"layers" 1
        )

  managedVulkanResource
    (Vulkan.vkCreateFramebuffer dev (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroyFramebuffer dev)

createDepthImage
  :: MonadManaged m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkFormat
  -> Vulkan.VkExtent2D
  -> m Vulkan.VkImage
createDepthImage physicalDevice device depthFormat extent = do
  let
    extent3d =
      -- TODO
               Vulkan.createVk
      (  Vulkan.set @"width" (Vulkan.getField @"width" extent)
      &* Vulkan.set @"height" (Vulkan.getField @"height" extent)
      &* Vulkan.set @"depth" 1
      )

    createInfo = Vulkan.createVk
      (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
      &* Vulkan.set @"pNext" Vulkan.VK_NULL
      &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
      &* Vulkan.set @"imageType" Vulkan.VK_IMAGE_TYPE_2D
      &* Vulkan.set @"format" depthFormat
      &* Vulkan.set @"extent" extent3d
      &* Vulkan.set @"mipLevels" 1
      &* Vulkan.set @"arrayLayers" 1
      &* Vulkan.set @"samples" Vulkan.VK_SAMPLE_COUNT_1_BIT
      &* Vulkan.set @"tiling" Vulkan.VK_IMAGE_TILING_OPTIMAL
      &* Vulkan.set @"usage"
           Vulkan.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
      &* Vulkan.set @"sharingMode" Vulkan.VK_SHARING_MODE_EXCLUSIVE
      &* Vulkan.set @"queueFamilyIndexCount" 0
      &* Vulkan.set @"pQueueFamilyIndices" Vulkan.VK_NULL
      &* Vulkan.set @"initialLayout" Vulkan.VK_IMAGE_LAYOUT_UNDEFINED
      )

  image <- managedVulkanResource
    (Vulkan.vkCreateImage device (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroyImage device)

  memoryRequirements <- allocaAndPeek
    (Vulkan.vkGetImageMemoryRequirements device image)

  memory <- allocateMemoryFor physicalDevice device memoryRequirements []

  liftIO (Vulkan.vkBindImageMemory device image memory 0 >>= throwVkResult)

  return image

allocateMemoryFor
  :: MonadIO m
  => Vulkan.VkPhysicalDevice
  -> Vulkan.VkDevice
  -> Vulkan.VkMemoryRequirements
  -> [Vulkan.VkMemoryPropertyFlags]
  -> m Vulkan.VkDeviceMemory
allocateMemoryFor physicalDevice device requirements requiredFlags = do
  memoryProperties <- allocaAndPeek
    (Vulkan.vkGetPhysicalDeviceMemoryProperties physicalDevice)

  let memoryTypeCount = Vulkan.getField @"memoryTypeCount" memoryProperties

  memoryTypes <- liftIO $ Foreign.Marshal.peekArray @Vulkan.VkMemoryType
    (fromIntegral memoryTypeCount)
    (                 Vulkan.unsafePtr memoryProperties
    `Foreign.plusPtr` Vulkan.fieldOffset @"memoryTypes"
                      @Vulkan.VkPhysicalDeviceMemoryProperties
    )

  let possibleMemoryTypeIndices = do
        (i, memoryType) <- zip [0 ..] memoryTypes

        guard
          (testBit (Vulkan.getField @"memoryTypeBits" requirements)
                   (fromIntegral i)
          )

        for_
          requiredFlags
          (\f -> guard
            (   Vulkan.getField @"propertyFlags" memoryType
            .&. f
            /=  Vulkan.VK_ZERO_FLAGS
            )
          )

        return i

  memoryTypeIndex <- case possibleMemoryTypeIndices of
    []      -> fail "No possible memory types"

    (i : _) -> return i

  let allocateInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"allocationSize" (Vulkan.getField @"size" requirements)
        &* Vulkan.set @"memoryTypeIndex" memoryTypeIndex
        )

  allocaAndPeek
    (   Vulkan.vkAllocateMemory device
                                (Vulkan.unsafePtr allocateInfo)
                                Vulkan.VK_NULL_HANDLE
    >=> throwVkResult
    )

createImageView
  :: MonadManaged m
  => Vulkan.VkDevice
  -> Vulkan.VkImage
  -> Vulkan.VkFormat
  -> Vulkan.VkImageAspectBitmask Vulkan.FlagMask
  -> m Vulkan.VkImageView
createImageView dev image format aspectMask = do
  let components :: Vulkan.VkComponentMapping = Vulkan.createVk
        (  Vulkan.set @"r" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"g" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"b" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        &* Vulkan.set @"a" Vulkan.VK_COMPONENT_SWIZZLE_IDENTITY
        )

      subResourceRange :: Vulkan.VkImageSubresourceRange = Vulkan.createVk
        (  Vulkan.set @"aspectMask" aspectMask
        &* Vulkan.set @"baseMipLevel" 0
        &* Vulkan.set @"levelCount" 1
        &* Vulkan.set @"baseArrayLayer" 0
        &* Vulkan.set @"layerCount" 1
        )

      createInfo :: Vulkan.VkImageViewCreateInfo = Vulkan.createVk
        (  Vulkan.set @"sType" Vulkan.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
        &* Vulkan.set @"pNext" Vulkan.VK_NULL
        &* Vulkan.set @"flags" Vulkan.VK_ZERO_FLAGS
        &* Vulkan.set @"image" image
        &* Vulkan.set @"viewType" Vulkan.VK_IMAGE_VIEW_TYPE_2D
        &* Vulkan.set @"format" format
        &* Vulkan.set @"components" components
        &* Vulkan.set @"subresourceRange" subResourceRange
        )

  managedVulkanResource
    (Vulkan.vkCreateImageView dev (Vulkan.unsafePtr createInfo))
    (Vulkan.vkDestroyImageView dev)

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
