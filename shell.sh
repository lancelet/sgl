#!/usr/bin/env bash

set -euo pipefail

# Download Vulkan SDK for MacOS if we're on that platform
if [[ "$OSTYPE" == "darwin"* ]]; then
    # if you bump the version here, also change the directory names in stack.yaml
    readonly vulkan_sdk_version='1.1.130.0'
    readonly script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
    readonly extras_dir="$script_dir/extras"
    readonly vulkan_sdk_dir="$extras_dir/vulkansdk-macos-$vulkan_sdk_version"
    if [ ! -d "$vulkan_sdk_dir" ]; then
        echo "Downloading and extracting the MacOS Vulkan SDK to $vulkan_sdk_dir"
        readonly base_url='https://sdk.lunarg.com/sdk/download'
        readonly sdk_url="$base_url/$vulkan_sdk_version/mac/vulkansdk-macos-$vulkan_sdk_version.tar.gz?u="
        mkdir -p "$extras_dir"
        pushd "$extras_dir"
        curl "$sdk_url" | tar xvz
        popd
    fi
    export VULKAN_SDK="$vulkan_sdk_dir/macOS"
    export VK_LAYER_PATH="$VULKAN_SDK/etc/vulkan/explicit_layer.d"
    export VK_ICD_FILENAMES="$VULKAN_SDK/etc/vulkan/icd.d/MoltenVK_icd.json"
    export PATH="$VULKAN_SDK/bin:$PATH"
    export DYLD_LIBRARY_PATH="$VULKAN_SDK/lib"
    echo 'Set the following environment variables:'
    echo "    VULKAN_SDK        '$VULKAN_SDK'"
    echo "    VK_LAYER_PATH     '$VK_LAYER_PATH'"
    echo "    VK_ICD_FILENAMES  '$VK_ICD_FILENAMES'"
    echo "    PATH              '$PATH'"
    echo "    DYLD_LIBRARY_PATH '$DYLD_LIBRARY_PATH'"
    install_name_tool -id "$VULKAN_SDK/lib/libvulkan.1.dylib" "$VULKAN_SDK/lib/libvulkan.1.dylib"
fi

# Set up a shell with SDL2
nix-shell -p pkgconfig SDL2 --command 'stack build --only-dependencies && stack hoogle --rebuild'
