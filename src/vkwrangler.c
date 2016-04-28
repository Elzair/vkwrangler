#include <vkwrangler/vkwrangler.h>
extern PFN_vkVoidFunction vkGetInstanceProcAddr(VkInstance instance, const char* pName);
extern PFN_vkVoidFunction vkGetDeviceProcAddr(VkDevice device, const char* pName);

void vkWranglerInitInstanceFunctions(VkInstance instance, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames) {
  vkCreateInstance = (VkResult (*)(const VkInstanceCreateInfo *, const VkAllocationCallbacks *, VkInstance *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateInstance");
  vkDestroyInstance = (void (*)(VkInstance, const VkAllocationCallbacks *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkDestroyInstance");
  vkEnumeratePhysicalDevices = (VkResult (*)(VkInstance, uint32_t *, VkPhysicalDevice *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkEnumeratePhysicalDevices");
  vkGetPhysicalDeviceProperties = (void (*)(VkPhysicalDevice, VkPhysicalDeviceProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceProperties");
  vkGetPhysicalDeviceQueueFamilyProperties = (void (*)(VkPhysicalDevice, uint32_t *, VkQueueFamilyProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceQueueFamilyProperties");
  vkGetPhysicalDeviceMemoryProperties = (void (*)(VkPhysicalDevice, VkPhysicalDeviceMemoryProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceMemoryProperties");
  vkGetPhysicalDeviceFeatures = (void (*)(VkPhysicalDevice, VkPhysicalDeviceFeatures *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceFeatures");
  vkGetPhysicalDeviceFormatProperties = (void (*)(VkPhysicalDevice, VkFormat, VkFormatProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceFormatProperties");
  vkGetPhysicalDeviceImageFormatProperties = (VkResult (*)(VkPhysicalDevice, VkFormat, VkImageType, VkImageTiling, VkImageUsageFlags, VkImageCreateFlags, VkImageFormatProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceImageFormatProperties");
  vkCreateDevice = (VkResult (*)(VkPhysicalDevice, const VkDeviceCreateInfo *, const VkAllocationCallbacks *, VkDevice *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateDevice");
  vkEnumerateInstanceLayerProperties = (VkResult (*)(uint32_t *, VkLayerProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkEnumerateInstanceLayerProperties");
  vkEnumerateInstanceExtensionProperties = (VkResult (*)(const char *, uint32_t *, VkExtensionProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkEnumerateInstanceExtensionProperties");
  vkEnumerateDeviceLayerProperties = (VkResult (*)(VkPhysicalDevice, uint32_t *, VkLayerProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkEnumerateDeviceLayerProperties");
  vkEnumerateDeviceExtensionProperties = (VkResult (*)(VkPhysicalDevice, const char *, uint32_t *, VkExtensionProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkEnumerateDeviceExtensionProperties");
  vkGetPhysicalDeviceSparseImageFormatProperties = (void (*)(VkPhysicalDevice, VkFormat, VkImageType, VkSampleCountFlagBits, VkImageUsageFlags, VkImageTiling, uint32_t *, VkSparseImageFormatProperties *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceSparseImageFormatProperties");
  uint32_t i;
  for (i=0; i < enabledExtensionNameCount; i++) {
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_surface") == 0) {
      vkDestroySurfaceKHR = (void (*)(VkInstance, VkSurfaceKHR, const VkAllocationCallbacks *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkDestroySurfaceKHR");
      vkGetPhysicalDeviceSurfaceSupportKHR = (VkResult (*)(VkPhysicalDevice, uint32_t, VkSurfaceKHR, VkBool32 *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceSurfaceSupportKHR");
      vkGetPhysicalDeviceSurfaceCapabilitiesKHR = (VkResult (*)(VkPhysicalDevice, VkSurfaceKHR, VkSurfaceCapabilitiesKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceSurfaceCapabilitiesKHR");
      vkGetPhysicalDeviceSurfaceFormatsKHR = (VkResult (*)(VkPhysicalDevice, VkSurfaceKHR, uint32_t *, VkSurfaceFormatKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceSurfaceFormatsKHR");
      vkGetPhysicalDeviceSurfacePresentModesKHR = (VkResult (*)(VkPhysicalDevice, VkSurfaceKHR, uint32_t *, VkPresentModeKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceSurfacePresentModesKHR");
    }

    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_display") == 0) {
      vkGetPhysicalDeviceDisplayPropertiesKHR = (VkResult (*)(VkPhysicalDevice, uint32_t *, VkDisplayPropertiesKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceDisplayPropertiesKHR");
      vkGetPhysicalDeviceDisplayPlanePropertiesKHR = (VkResult (*)(VkPhysicalDevice, uint32_t *, VkDisplayPlanePropertiesKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceDisplayPlanePropertiesKHR");
      vkGetDisplayPlaneSupportedDisplaysKHR = (VkResult (*)(VkPhysicalDevice, uint32_t, uint32_t *, VkDisplayKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetDisplayPlaneSupportedDisplaysKHR");
      vkGetDisplayModePropertiesKHR = (VkResult (*)(VkPhysicalDevice, VkDisplayKHR, uint32_t *, VkDisplayModePropertiesKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetDisplayModePropertiesKHR");
      vkCreateDisplayModeKHR = (VkResult (*)(VkPhysicalDevice, VkDisplayKHR, const VkDisplayModeCreateInfoKHR *, const VkAllocationCallbacks *, VkDisplayModeKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateDisplayModeKHR");
      vkGetDisplayPlaneCapabilitiesKHR = (VkResult (*)(VkPhysicalDevice, VkDisplayModeKHR, uint32_t, VkDisplayPlaneCapabilitiesKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetDisplayPlaneCapabilitiesKHR");
      vkCreateDisplayPlaneSurfaceKHR = (VkResult (*)(VkInstance, const VkDisplaySurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateDisplayPlaneSurfaceKHR");
    }

#ifdef VK_USE_PLATFORM_XLIB_KHR
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_xlib_surface") == 0) {
      vkCreateXlibSurfaceKHR = (VkResult (*)(VkInstance, const VkXlibSurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateXlibSurfaceKHR");
      vkGetPhysicalDeviceXlibPresentationSupportKHR = (VkBool32 (*)(VkPhysicalDevice, uint32_t, Display *, VisualID))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceXlibPresentationSupportKHR");
    }
#endif
#ifdef VK_USE_PLATFORM_XCB_KHR
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_xcb_surface") == 0) {
      vkCreateXcbSurfaceKHR = (VkResult (*)(VkInstance, const VkXcbSurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateXcbSurfaceKHR");
      vkGetPhysicalDeviceXcbPresentationSupportKHR = (VkBool32 (*)(VkPhysicalDevice, uint32_t, xcb_connection_t *, xcb_visualid_t))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceXcbPresentationSupportKHR");
    }
#endif
#ifdef VK_USE_PLATFORM_WAYLAND_KHR
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_wayland_surface") == 0) {
      vkCreateWaylandSurfaceKHR = (VkResult (*)(VkInstance, const VkWaylandSurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateWaylandSurfaceKHR");
      vkGetPhysicalDeviceWaylandPresentationSupportKHR = (VkBool32 (*)(VkPhysicalDevice, uint32_t, struct wl_display *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceWaylandPresentationSupportKHR");
    }
#endif
#ifdef VK_USE_PLATFORM_MIR_KHR
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_mir_surface") == 0) {
      vkCreateMirSurfaceKHR = (VkResult (*)(VkInstance, const VkMirSurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateMirSurfaceKHR");
      vkGetPhysicalDeviceMirPresentationSupportKHR = (VkBool32 (*)(VkPhysicalDevice, uint32_t, MirConnection *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceMirPresentationSupportKHR");
    }
#endif
#ifdef VK_USE_PLATFORM_ANDROID_KHR
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_android_surface") == 0) {
      vkCreateAndroidSurfaceKHR = (VkResult (*)(VkInstance, const VkAndroidSurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateAndroidSurfaceKHR");
    }
#endif
#ifdef VK_USE_PLATFORM_WIN32_KHR
    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_win32_surface") == 0) {
      vkCreateWin32SurfaceKHR = (VkResult (*)(VkInstance, const VkWin32SurfaceCreateInfoKHR *, const VkAllocationCallbacks *, VkSurfaceKHR *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateWin32SurfaceKHR");
      vkGetPhysicalDeviceWin32PresentationSupportKHR = (VkBool32 (*)(VkPhysicalDevice, uint32_t))(uintptr_t)vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceWin32PresentationSupportKHR");
    }
#endif
    if (strcmp(ppEnabledExtensionNames[i], "VK_EXT_debug_report") == 0) {
      vkCreateDebugReportCallbackEXT = (VkResult (*)(VkInstance, const VkDebugReportCallbackCreateInfoEXT *, const VkAllocationCallbacks *, VkDebugReportCallbackEXT *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkCreateDebugReportCallbackEXT");
      vkDestroyDebugReportCallbackEXT = (void (*)(VkInstance, VkDebugReportCallbackEXT, const VkAllocationCallbacks *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkDestroyDebugReportCallbackEXT");
      vkDebugReportMessageEXT = (void (*)(VkInstance, VkDebugReportFlagsEXT, VkDebugReportObjectTypeEXT, uint64_t, size_t, int32_t, const char *, const char *))(uintptr_t)vkGetInstanceProcAddr(instance, "vkDebugReportMessageEXT");
    }



  }
}

void vkWranglerInitDeviceFunctions(VkDevice device, uint32_t enabledExtensionNameCount, const char* const* ppEnabledExtensionNames) {
  vkDestroyDevice = (void (*)(VkDevice, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyDevice");
  vkGetDeviceQueue = (void (*)(VkDevice, uint32_t, uint32_t, VkQueue *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetDeviceQueue");
  vkQueueSubmit = (VkResult (*)(VkQueue, uint32_t, const VkSubmitInfo *, VkFence))(uintptr_t)vkGetDeviceProcAddr(device, "vkQueueSubmit");
  vkQueueWaitIdle = (VkResult (*)(VkQueue))(uintptr_t)vkGetDeviceProcAddr(device, "vkQueueWaitIdle");
  vkDeviceWaitIdle = (VkResult (*)(VkDevice))(uintptr_t)vkGetDeviceProcAddr(device, "vkDeviceWaitIdle");
  vkAllocateMemory = (VkResult (*)(VkDevice, const VkMemoryAllocateInfo *, const VkAllocationCallbacks *, VkDeviceMemory *))(uintptr_t)vkGetDeviceProcAddr(device, "vkAllocateMemory");
  vkFreeMemory = (void (*)(VkDevice, VkDeviceMemory, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkFreeMemory");
  vkMapMemory = (VkResult (*)(VkDevice, VkDeviceMemory, VkDeviceSize, VkDeviceSize, VkMemoryMapFlags, void **))(uintptr_t)vkGetDeviceProcAddr(device, "vkMapMemory");
  vkUnmapMemory = (void (*)(VkDevice, VkDeviceMemory))(uintptr_t)vkGetDeviceProcAddr(device, "vkUnmapMemory");
  vkFlushMappedMemoryRanges = (VkResult (*)(VkDevice, uint32_t, const VkMappedMemoryRange *))(uintptr_t)vkGetDeviceProcAddr(device, "vkFlushMappedMemoryRanges");
  vkInvalidateMappedMemoryRanges = (VkResult (*)(VkDevice, uint32_t, const VkMappedMemoryRange *))(uintptr_t)vkGetDeviceProcAddr(device, "vkInvalidateMappedMemoryRanges");
  vkGetDeviceMemoryCommitment = (void (*)(VkDevice, VkDeviceMemory, VkDeviceSize *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetDeviceMemoryCommitment");
  vkGetBufferMemoryRequirements = (void (*)(VkDevice, VkBuffer, VkMemoryRequirements *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetBufferMemoryRequirements");
  vkBindBufferMemory = (VkResult (*)(VkDevice, VkBuffer, VkDeviceMemory, VkDeviceSize))(uintptr_t)vkGetDeviceProcAddr(device, "vkBindBufferMemory");
  vkGetImageMemoryRequirements = (void (*)(VkDevice, VkImage, VkMemoryRequirements *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetImageMemoryRequirements");
  vkBindImageMemory = (VkResult (*)(VkDevice, VkImage, VkDeviceMemory, VkDeviceSize))(uintptr_t)vkGetDeviceProcAddr(device, "vkBindImageMemory");
  vkGetImageSparseMemoryRequirements = (void (*)(VkDevice, VkImage, uint32_t *, VkSparseImageMemoryRequirements *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetImageSparseMemoryRequirements");
  vkQueueBindSparse = (VkResult (*)(VkQueue, uint32_t, const VkBindSparseInfo *, VkFence))(uintptr_t)vkGetDeviceProcAddr(device, "vkQueueBindSparse");
  vkCreateFence = (VkResult (*)(VkDevice, const VkFenceCreateInfo *, const VkAllocationCallbacks *, VkFence *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateFence");
  vkDestroyFence = (void (*)(VkDevice, VkFence, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyFence");
  vkResetFences = (VkResult (*)(VkDevice, uint32_t, const VkFence *))(uintptr_t)vkGetDeviceProcAddr(device, "vkResetFences");
  vkGetFenceStatus = (VkResult (*)(VkDevice, VkFence))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetFenceStatus");
  vkWaitForFences = (VkResult (*)(VkDevice, uint32_t, const VkFence *, VkBool32, uint64_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkWaitForFences");
  vkCreateSemaphore = (VkResult (*)(VkDevice, const VkSemaphoreCreateInfo *, const VkAllocationCallbacks *, VkSemaphore *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateSemaphore");
  vkDestroySemaphore = (void (*)(VkDevice, VkSemaphore, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroySemaphore");
  vkCreateEvent = (VkResult (*)(VkDevice, const VkEventCreateInfo *, const VkAllocationCallbacks *, VkEvent *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateEvent");
  vkDestroyEvent = (void (*)(VkDevice, VkEvent, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyEvent");
  vkGetEventStatus = (VkResult (*)(VkDevice, VkEvent))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetEventStatus");
  vkSetEvent = (VkResult (*)(VkDevice, VkEvent))(uintptr_t)vkGetDeviceProcAddr(device, "vkSetEvent");
  vkResetEvent = (VkResult (*)(VkDevice, VkEvent))(uintptr_t)vkGetDeviceProcAddr(device, "vkResetEvent");
  vkCreateQueryPool = (VkResult (*)(VkDevice, const VkQueryPoolCreateInfo *, const VkAllocationCallbacks *, VkQueryPool *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateQueryPool");
  vkDestroyQueryPool = (void (*)(VkDevice, VkQueryPool, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyQueryPool");
  vkGetQueryPoolResults = (VkResult (*)(VkDevice, VkQueryPool, uint32_t, uint32_t, size_t, void *, VkDeviceSize, VkQueryResultFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetQueryPoolResults");
  vkCreateBuffer = (VkResult (*)(VkDevice, const VkBufferCreateInfo *, const VkAllocationCallbacks *, VkBuffer *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateBuffer");
  vkDestroyBuffer = (void (*)(VkDevice, VkBuffer, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyBuffer");
  vkCreateBufferView = (VkResult (*)(VkDevice, const VkBufferViewCreateInfo *, const VkAllocationCallbacks *, VkBufferView *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateBufferView");
  vkDestroyBufferView = (void (*)(VkDevice, VkBufferView, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyBufferView");
  vkCreateImage = (VkResult (*)(VkDevice, const VkImageCreateInfo *, const VkAllocationCallbacks *, VkImage *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateImage");
  vkDestroyImage = (void (*)(VkDevice, VkImage, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyImage");
  vkGetImageSubresourceLayout = (void (*)(VkDevice, VkImage, const VkImageSubresource *, VkSubresourceLayout *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetImageSubresourceLayout");
  vkCreateImageView = (VkResult (*)(VkDevice, const VkImageViewCreateInfo *, const VkAllocationCallbacks *, VkImageView *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateImageView");
  vkDestroyImageView = (void (*)(VkDevice, VkImageView, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyImageView");
  vkCreateShaderModule = (VkResult (*)(VkDevice, const VkShaderModuleCreateInfo *, const VkAllocationCallbacks *, VkShaderModule *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateShaderModule");
  vkDestroyShaderModule = (void (*)(VkDevice, VkShaderModule, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyShaderModule");
  vkCreatePipelineCache = (VkResult (*)(VkDevice, const VkPipelineCacheCreateInfo *, const VkAllocationCallbacks *, VkPipelineCache *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreatePipelineCache");
  vkDestroyPipelineCache = (void (*)(VkDevice, VkPipelineCache, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyPipelineCache");
  vkGetPipelineCacheData = (VkResult (*)(VkDevice, VkPipelineCache, size_t *, void *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetPipelineCacheData");
  vkMergePipelineCaches = (VkResult (*)(VkDevice, VkPipelineCache, uint32_t, const VkPipelineCache *))(uintptr_t)vkGetDeviceProcAddr(device, "vkMergePipelineCaches");
  vkCreateGraphicsPipelines = (VkResult (*)(VkDevice, VkPipelineCache, uint32_t, const VkGraphicsPipelineCreateInfo *, const VkAllocationCallbacks *, VkPipeline *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateGraphicsPipelines");
  vkCreateComputePipelines = (VkResult (*)(VkDevice, VkPipelineCache, uint32_t, const VkComputePipelineCreateInfo *, const VkAllocationCallbacks *, VkPipeline *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateComputePipelines");
  vkDestroyPipeline = (void (*)(VkDevice, VkPipeline, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyPipeline");
  vkCreatePipelineLayout = (VkResult (*)(VkDevice, const VkPipelineLayoutCreateInfo *, const VkAllocationCallbacks *, VkPipelineLayout *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreatePipelineLayout");
  vkDestroyPipelineLayout = (void (*)(VkDevice, VkPipelineLayout, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyPipelineLayout");
  vkCreateSampler = (VkResult (*)(VkDevice, const VkSamplerCreateInfo *, const VkAllocationCallbacks *, VkSampler *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateSampler");
  vkDestroySampler = (void (*)(VkDevice, VkSampler, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroySampler");
  vkCreateDescriptorSetLayout = (VkResult (*)(VkDevice, const VkDescriptorSetLayoutCreateInfo *, const VkAllocationCallbacks *, VkDescriptorSetLayout *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateDescriptorSetLayout");
  vkDestroyDescriptorSetLayout = (void (*)(VkDevice, VkDescriptorSetLayout, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyDescriptorSetLayout");
  vkCreateDescriptorPool = (VkResult (*)(VkDevice, const VkDescriptorPoolCreateInfo *, const VkAllocationCallbacks *, VkDescriptorPool *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateDescriptorPool");
  vkDestroyDescriptorPool = (void (*)(VkDevice, VkDescriptorPool, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyDescriptorPool");
  vkResetDescriptorPool = (VkResult (*)(VkDevice, VkDescriptorPool, VkDescriptorPoolResetFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkResetDescriptorPool");
  vkAllocateDescriptorSets = (VkResult (*)(VkDevice, const VkDescriptorSetAllocateInfo *, VkDescriptorSet *))(uintptr_t)vkGetDeviceProcAddr(device, "vkAllocateDescriptorSets");
  vkFreeDescriptorSets = (VkResult (*)(VkDevice, VkDescriptorPool, uint32_t, const VkDescriptorSet *))(uintptr_t)vkGetDeviceProcAddr(device, "vkFreeDescriptorSets");
  vkUpdateDescriptorSets = (void (*)(VkDevice, uint32_t, const VkWriteDescriptorSet *, uint32_t, const VkCopyDescriptorSet *))(uintptr_t)vkGetDeviceProcAddr(device, "vkUpdateDescriptorSets");
  vkCreateFramebuffer = (VkResult (*)(VkDevice, const VkFramebufferCreateInfo *, const VkAllocationCallbacks *, VkFramebuffer *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateFramebuffer");
  vkDestroyFramebuffer = (void (*)(VkDevice, VkFramebuffer, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyFramebuffer");
  vkCreateRenderPass = (VkResult (*)(VkDevice, const VkRenderPassCreateInfo *, const VkAllocationCallbacks *, VkRenderPass *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateRenderPass");
  vkDestroyRenderPass = (void (*)(VkDevice, VkRenderPass, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyRenderPass");
  vkGetRenderAreaGranularity = (void (*)(VkDevice, VkRenderPass, VkExtent2D *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetRenderAreaGranularity");
  vkCreateCommandPool = (VkResult (*)(VkDevice, const VkCommandPoolCreateInfo *, const VkAllocationCallbacks *, VkCommandPool *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateCommandPool");
  vkDestroyCommandPool = (void (*)(VkDevice, VkCommandPool, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroyCommandPool");
  vkResetCommandPool = (VkResult (*)(VkDevice, VkCommandPool, VkCommandPoolResetFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkResetCommandPool");
  vkAllocateCommandBuffers = (VkResult (*)(VkDevice, const VkCommandBufferAllocateInfo *, VkCommandBuffer *))(uintptr_t)vkGetDeviceProcAddr(device, "vkAllocateCommandBuffers");
  vkFreeCommandBuffers = (void (*)(VkDevice, VkCommandPool, uint32_t, const VkCommandBuffer *))(uintptr_t)vkGetDeviceProcAddr(device, "vkFreeCommandBuffers");
  vkBeginCommandBuffer = (VkResult (*)(VkCommandBuffer, const VkCommandBufferBeginInfo *))(uintptr_t)vkGetDeviceProcAddr(device, "vkBeginCommandBuffer");
  vkEndCommandBuffer = (VkResult (*)(VkCommandBuffer))(uintptr_t)vkGetDeviceProcAddr(device, "vkEndCommandBuffer");
  vkResetCommandBuffer = (VkResult (*)(VkCommandBuffer, VkCommandBufferResetFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkResetCommandBuffer");
  vkCmdBindPipeline = (void (*)(VkCommandBuffer, VkPipelineBindPoint, VkPipeline))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBindPipeline");
  vkCmdSetViewport = (void (*)(VkCommandBuffer, uint32_t, uint32_t, const VkViewport *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetViewport");
  vkCmdSetScissor = (void (*)(VkCommandBuffer, uint32_t, uint32_t, const VkRect2D *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetScissor");
  vkCmdSetLineWidth = (void (*)(VkCommandBuffer, float))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetLineWidth");
  vkCmdSetDepthBias = (void (*)(VkCommandBuffer, float, float, float))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetDepthBias");
  vkCmdSetBlendConstants = (void (*)(VkCommandBuffer, const float *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetBlendConstants");
  vkCmdSetDepthBounds = (void (*)(VkCommandBuffer, float, float))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetDepthBounds");
  vkCmdSetStencilCompareMask = (void (*)(VkCommandBuffer, VkStencilFaceFlags, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetStencilCompareMask");
  vkCmdSetStencilWriteMask = (void (*)(VkCommandBuffer, VkStencilFaceFlags, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetStencilWriteMask");
  vkCmdSetStencilReference = (void (*)(VkCommandBuffer, VkStencilFaceFlags, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetStencilReference");
  vkCmdBindDescriptorSets = (void (*)(VkCommandBuffer, VkPipelineBindPoint, VkPipelineLayout, uint32_t, uint32_t, const VkDescriptorSet *, uint32_t, const uint32_t *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBindDescriptorSets");
  vkCmdBindIndexBuffer = (void (*)(VkCommandBuffer, VkBuffer, VkDeviceSize, VkIndexType))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBindIndexBuffer");
  vkCmdBindVertexBuffers = (void (*)(VkCommandBuffer, uint32_t, uint32_t, const VkBuffer *, const VkDeviceSize *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBindVertexBuffers");
  vkCmdDraw = (void (*)(VkCommandBuffer, uint32_t, uint32_t, uint32_t, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdDraw");
  vkCmdDrawIndexed = (void (*)(VkCommandBuffer, uint32_t, uint32_t, uint32_t, int32_t, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdDrawIndexed");
  vkCmdDrawIndirect = (void (*)(VkCommandBuffer, VkBuffer, VkDeviceSize, uint32_t, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdDrawIndirect");
  vkCmdDrawIndexedIndirect = (void (*)(VkCommandBuffer, VkBuffer, VkDeviceSize, uint32_t, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdDrawIndexedIndirect");
  vkCmdDispatch = (void (*)(VkCommandBuffer, uint32_t, uint32_t, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdDispatch");
  vkCmdDispatchIndirect = (void (*)(VkCommandBuffer, VkBuffer, VkDeviceSize))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdDispatchIndirect");
  vkCmdCopyBuffer = (void (*)(VkCommandBuffer, VkBuffer, VkBuffer, uint32_t, const VkBufferCopy *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdCopyBuffer");
  vkCmdCopyImage = (void (*)(VkCommandBuffer, VkImage, VkImageLayout, VkImage, VkImageLayout, uint32_t, const VkImageCopy *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdCopyImage");
  vkCmdBlitImage = (void (*)(VkCommandBuffer, VkImage, VkImageLayout, VkImage, VkImageLayout, uint32_t, const VkImageBlit *, VkFilter))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBlitImage");
  vkCmdCopyBufferToImage = (void (*)(VkCommandBuffer, VkBuffer, VkImage, VkImageLayout, uint32_t, const VkBufferImageCopy *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdCopyBufferToImage");
  vkCmdCopyImageToBuffer = (void (*)(VkCommandBuffer, VkImage, VkImageLayout, VkBuffer, uint32_t, const VkBufferImageCopy *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdCopyImageToBuffer");
  vkCmdUpdateBuffer = (void (*)(VkCommandBuffer, VkBuffer, VkDeviceSize, VkDeviceSize, const uint32_t *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdUpdateBuffer");
  vkCmdFillBuffer = (void (*)(VkCommandBuffer, VkBuffer, VkDeviceSize, VkDeviceSize, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdFillBuffer");
  vkCmdClearColorImage = (void (*)(VkCommandBuffer, VkImage, VkImageLayout, const VkClearColorValue *, uint32_t, const VkImageSubresourceRange *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdClearColorImage");
  vkCmdClearDepthStencilImage = (void (*)(VkCommandBuffer, VkImage, VkImageLayout, const VkClearDepthStencilValue *, uint32_t, const VkImageSubresourceRange *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdClearDepthStencilImage");
  vkCmdClearAttachments = (void (*)(VkCommandBuffer, uint32_t, const VkClearAttachment *, uint32_t, const VkClearRect *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdClearAttachments");
  vkCmdResolveImage = (void (*)(VkCommandBuffer, VkImage, VkImageLayout, VkImage, VkImageLayout, uint32_t, const VkImageResolve *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdResolveImage");
  vkCmdSetEvent = (void (*)(VkCommandBuffer, VkEvent, VkPipelineStageFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdSetEvent");
  vkCmdResetEvent = (void (*)(VkCommandBuffer, VkEvent, VkPipelineStageFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdResetEvent");
  vkCmdWaitEvents = (void (*)(VkCommandBuffer, uint32_t, const VkEvent *, VkPipelineStageFlags, VkPipelineStageFlags, uint32_t, const VkMemoryBarrier *, uint32_t, const VkBufferMemoryBarrier *, uint32_t, const VkImageMemoryBarrier *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdWaitEvents");
  vkCmdPipelineBarrier = (void (*)(VkCommandBuffer, VkPipelineStageFlags, VkPipelineStageFlags, VkDependencyFlags, uint32_t, const VkMemoryBarrier *, uint32_t, const VkBufferMemoryBarrier *, uint32_t, const VkImageMemoryBarrier *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdPipelineBarrier");
  vkCmdBeginQuery = (void (*)(VkCommandBuffer, VkQueryPool, uint32_t, VkQueryControlFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBeginQuery");
  vkCmdEndQuery = (void (*)(VkCommandBuffer, VkQueryPool, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdEndQuery");
  vkCmdResetQueryPool = (void (*)(VkCommandBuffer, VkQueryPool, uint32_t, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdResetQueryPool");
  vkCmdWriteTimestamp = (void (*)(VkCommandBuffer, VkPipelineStageFlagBits, VkQueryPool, uint32_t))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdWriteTimestamp");
  vkCmdCopyQueryPoolResults = (void (*)(VkCommandBuffer, VkQueryPool, uint32_t, uint32_t, VkBuffer, VkDeviceSize, VkDeviceSize, VkQueryResultFlags))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdCopyQueryPoolResults");
  vkCmdPushConstants = (void (*)(VkCommandBuffer, VkPipelineLayout, VkShaderStageFlags, uint32_t, uint32_t, const void *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdPushConstants");
  vkCmdBeginRenderPass = (void (*)(VkCommandBuffer, const VkRenderPassBeginInfo *, VkSubpassContents))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdBeginRenderPass");
  vkCmdNextSubpass = (void (*)(VkCommandBuffer, VkSubpassContents))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdNextSubpass");
  vkCmdEndRenderPass = (void (*)(VkCommandBuffer))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdEndRenderPass");
  vkCmdExecuteCommands = (void (*)(VkCommandBuffer, uint32_t, const VkCommandBuffer *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCmdExecuteCommands");
  uint32_t i;
  for (i=0; i < enabledExtensionNameCount; i++) {

    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_swapchain") == 0) {
      vkCreateSwapchainKHR = (VkResult (*)(VkDevice, const VkSwapchainCreateInfoKHR *, const VkAllocationCallbacks *, VkSwapchainKHR *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateSwapchainKHR");
      vkDestroySwapchainKHR = (void (*)(VkDevice, VkSwapchainKHR, const VkAllocationCallbacks *))(uintptr_t)vkGetDeviceProcAddr(device, "vkDestroySwapchainKHR");
      vkGetSwapchainImagesKHR = (VkResult (*)(VkDevice, VkSwapchainKHR, uint32_t *, VkImage *))(uintptr_t)vkGetDeviceProcAddr(device, "vkGetSwapchainImagesKHR");
      vkAcquireNextImageKHR = (VkResult (*)(VkDevice, VkSwapchainKHR, uint64_t, VkSemaphore, VkFence, uint32_t *))(uintptr_t)vkGetDeviceProcAddr(device, "vkAcquireNextImageKHR");
      vkQueuePresentKHR = (VkResult (*)(VkQueue, const VkPresentInfoKHR *))(uintptr_t)vkGetDeviceProcAddr(device, "vkQueuePresentKHR");
    }

    if (strcmp(ppEnabledExtensionNames[i], "VK_KHR_display_swapchain") == 0) {
      vkCreateSharedSwapchainsKHR = (VkResult (*)(VkDevice, uint32_t, const VkSwapchainCreateInfoKHR *, const VkAllocationCallbacks *, VkSwapchainKHR *))(uintptr_t)vkGetDeviceProcAddr(device, "vkCreateSharedSwapchainsKHR");
    }










  }
}
