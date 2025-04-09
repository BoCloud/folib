# 工具概述

`Folib` 同时提供了一个 `CLI` 工具，基于 `Go` 语言编写，用于从容器镜像和文件系统生成软件物料清单（ `SBOM` ）。当与制品库、安全扫描工具程序一起使用时，就能够发挥它最大的价值。

## 特性
- 为源码、通用制品、容器镜像、文件系统、存档等生成 *SBOM* ，以发现依赖包以及 *license* 信息；
- 在 *SBOM* 格式之间进行转换，例如 *CycloneDX* 、*SPDX* 和 *Folib* 自己的格式；
- 能够与服务端交互，在 *CI* 阶段识别扫描代码、制品、镜像的 *SBOM* 并上传到服务端进行解析；
- 容器镜像如采用 *folib-json* 格式还可识别基础镜像。

## 支持的生态系统
- Alpine (apk)
- C (conan)
- C++ (conan)
- Dart (pubs)
- Debian (dpkg)
- Dotnet (deps.json)
- Objective-C (cocoapods)
- Elixir (mix)
- Erlang (rebar3)
- Go (go.mod, Go binaries)
- Haskell (cabal, stack)
- Java (jar, ear, war, par, sar, nar, native-image)
- JavaScript (npm, yarn)
- Jenkins Plugins (jpi, hpi)
- Linux kernel archives (vmlinz)
- Linux kernel modules (ko)
- Nix (outputs in /nix/store)
- PHP (composer)
- Python (wheel, egg, poetry, requirements.txt)
- Red Hat (rpm)
- Ruby (gem)
- Rust (cargo.lock)
- Swift (cocoapods, swift-package-manager)

## 命令汇总

这里列出了常用的命令及格式，具体说明请看 **命令工具文档的操作指南部分** 。

```shell
# 登陆 >>>
## 直接登陆
folib server login -H [host-url] -u [username] -p [password]

## 交互登陆
folib server login -H [host-url]
# <<< 登陆

# 制品操作 >>>
## 上传制品
folib artifact upload -S [storageID] -r [repositoryID] -T [TargetPath] -f [filePath]

## 制品晋级
folib artifact promotion -S [source] -T [target]

## 上传 SBOM
folib artifact sbom <image> -o <format> -S [storageID] -r [repositoryID] -a [artifactPath]

## 分片上传制品
folib artifact chunk-upload -S [storageID] -R [repositoryID] -T [targetPath] -F [filePath] -C [chunkSize]
# <<< 制品操作

# 批量操作 >>>
# 文件夹递归上传
./folib artifact upload -S 仓库ID -r 存储库ID -T "目标路径"  -f "文件夹路径"

# 批量上传
./folib artifact upload -b 'json字符串（需转义）'

# 批量下载
./folib artifact download -b 'json字符串（需转义）'
# <<< 批量操作
```