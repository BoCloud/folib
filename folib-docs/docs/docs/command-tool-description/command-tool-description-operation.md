# 操作指南

## 安装下载

[🔗 Folib下载链接](https://public.folib.com/storages/folib/folib-raw/tools/latest/folib-cli.zip)

## 服务端交互命令

+ **登录**

在终端输入登录主命令：`folib server login` ，调用登录逻辑

可指定参数:
| 参数 | 参数解释 |
| :----: | :----: |
| `-H` | 指定主机URL |
| `-u` | 指定登录的用户名 |
| `-p` | 指定登录的密码 |

提供以下两种方式登录， **直接输入命令登录** 或者 **按照提示输入用户名密码登录**

- *直接登录*

```shell
folib server login -H [host-url] -u [username] -p [password]
```

:::tip
⚠️ 在命令行参数中使用密码可能存在安全风险，因为它们可能会在 `shell` 历史记录中可见
:::

- *交互式登录*

交互式登录方式用户名密码由用户在终端交互输入

```shell
folib server login -H [host-url]
```

+ **上传制品**

可指定参数：
| 参数 | 参数解释 |
| :----: | :----: |
| `-S` | 指定仓库 `ID` |
| `-r` | 指定存储库 `ID` |
| `-T` | 指定目标路径 |
| `-f` | 指定源路径 |

```shell
folib artifact upload -S [storageID] -r [repositoryID] -T [TargetPath] -f [filePath]
```

+ **制品晋级**

可指定参数：
| 参数 | 参数解释 |
| :----: | :----: |
| `-S` | 指定源仓库地址 |
| `-T` | 指定目标仓库地址 |

```shell
folib artifact promotion -S [source] -T [target]
```

+ **上传 SBOM**

使用以下命令扫描项目生成 `SBOM`文件并上传到开源治理与制品库中，使用 `image`指定项目的路径

可指定参数：
| 参数 | 参数解释 |
| :----: | :----: |
| `-S` | 指定仓库 `ID` |
| `-r` | 指定存储库 `ID` |
| `-a` | 指定存储路径 |

```shell
folib artifact sbom <image> -o <format> -S [storageID] -r [repositoryID] -a [artifactPath]
```

在 `format` 中定义输出的 `SBOM` 格式，以下是 `format` 中可选参数：
| 类型 | 类型格式 |
| :----: | :----: |
| `cyclonedx-xml` | 符合 [CycloneDX 1.4 规范](https://cyclonedx.org/specification/overview/)的 `XML` 格式 |
| `cyclonedx-json` | 符合 [CycloneDX 1.4 规范](https://cyclonedx.org/specification/overview/)的 `JSON` 格式 |
| `spdx-tag-value` | 符合 [SPDX 2.3 规范](https://spdx.github.io/spdx-spec/v2.3/)的标记值格式格式 |
| `spdx-tag-value@2.2` | 符合 [SPDX 2.2 规范](https://spdx.github.io/spdx-spec/v2.2.2/)的标记值格式格式 |
| `spdx-json` | 符合 [SPDX 2.3 JSON 架构的 JSON](https://github.com/spdx/spdx-spec/blob/v2.3/schemas/spdx-schema.json) 格式 |
| `spdx-json@2.2` | 符合 [SPDX 2.2 JSON 架构的 JSON](https://github.com/spdx/spdx-spec/blob/v2.2/schemas/spdx-schema.json) 格式 |
| `github-json` | 符合 `GitHub` 依赖快照格式的 `JSON` 格式 |

:::tip
**folib-json 格式说明**

- *folib-json* 格式能够进行对 *cyclonedx* 、 *spdx* 等格式进行转换
- 针对容器镜像能够生成和识别基础镜像
:::

+ **分片上传制品**

可指定参数：
| 参数 | 参数解释 |
| :----: | :----: |
| `-S` | 指定仓库 `ID` |
| `-R` | 指定存储库 `ID` |
| `-T` | 指定目标路径 |
| `-F` | 指定源路径 |
| `-C` | 指定分片的大小，若用户未指定分片大小，则分片大小默认为 `50MB` |

```shell
folib artifact chunk-upload -S [storageID] -R [repositoryID] -T [targetPath] -F [filePath] -C [chunkSize]
```

## SBOM生成器命令

要为容器镜像生成 `SBOM` ，请执行以下操作：
```shell
folib <image>
```

上述输出仅包括容器中可见的软件（即镜像的压缩表示形式）。要将所有镜像层的软件包含在 `SBOM` 中，无论其是否存在于最终镜像中，请提供 `--scope all-layers` (该命令只会生成 `sbom` 文件但不会主动上传服务端，如果需要上传服务端请采用 `sbom` 命令)：
```shell
folib <image> --scope all-layers
```

+ 支持的来源

`folib` 可以从各种文件生成 `SBOM`：

```shell
# 对容器镜像存档进行编目（根据“docker image save ...”、“podman save ...”或“skopeo copy”命令的结果）
folib path/to/image.tar

# 格式 （SIF） 容器
folib path/to/image.sif

# 目录文件夹，该方法用于源代码扫描
folib path/to/dir
```

+ 排除文件路径方法

`folib` 可以使用扫描的文件和路径中排除，替换为一个或多个 `--exclude` 参数：
```shell
folib <source> --exclude './out/**/*.json' --exclude /etc
```

:::tip
在 *_image scanning_* 的情况下，由于扫描了整个文件系统，因此它是可以使用绝对路径，如 */etc* 或 */usr/xx/x.txt* ，而 *_directory scans_* 排除 *_relative* 指定 *directory_* 的文件。例如：扫描 */usr/foo --exclude ./package.json* 将排除 */usr/foo/package.json* 和 *—exclude **/package.json’* 将排除 */usr/foo* 下的所有 *package.json* 文件。对于 *_directory scans_* ，路径表达式需要以 *./*、*x/* 或 *xx/* 开头，所有这些将 *_relative* 解析到指定的扫描 *directory_* 。请记住，你的外壳可能会尝试扩展通配符，因此请将这些参数放在单引号中，例如：*'xx/x.json’* 。
:::

+ 输出格式

输出格式可以通过 `-o` (or `--output`) 的参数指定:
```shell
folib <image> -o <format>
```

可用的`格式`包括：
| 类型 | 类型格式 |
| :----: | :----: |
| `folib-json` | 与开源治理平台服务端平台交互识别的私有化协议 |
| `cyclonedx-xml` | 符合 [CycloneDX 1.5 规范](https://cyclonedx.org/specification/overview/) 的 `XML` 报告|
| `cyclonedx-json` | 符合 [CycloneDX 1.5规范](https://cyclonedx.org/specification/overview/) 的 `JSON` 报告 |
| `spdx-json` | 符合 [SPDX 2.3 JSON Schema](https://github.com/spdx/spdx-spec/blob/v2.3/schemas/spdx-schema.json)的 `JSON` 报告 |
| `github-json` | 符合 `GitHub` 依赖快照格式的 `JSON` 报告 |

## 批量多种输出

folib 还可以通过附加 `=<file>` 到选项中，例如输出 `folib JSON` 和 `SPDX JSON` ：

```shell
folib <image> -o folib-json=folib.json -o spdx-json=spdx.json
# 可在本地保存sbom.json和spdx.json文件

folib <image>  --scope all-layers
```

## 格式转换

转换现有 `SBOM` 的能力意味着您可以快速创建不同格式的 `SBOM` ，而无需从头开始重新生成 `SBOM` ，这可能需要更多时间

```shell
folib convert <ORIGINAL-SBOM-FILE> -o <NEW-SBOM-FORMAT>[=<NEW-SBOM-FILE>]
```

此功能是实验性的，转换格式时可能会丢失数据。包是主要的 `SBOM` 组件，可以很容易地跨格式传输，而文件和关系以及 `folib` 不支持的其他信息更有可能丢失。

我们支持具有广泛社区使用的格式，以及 `FOLIB` 的良好编码/解码支持。支持的格式有：
| 类型 | 参数配置 |
| :----: | :----: |
| `Folib JSON` | `-o folib-json` |
| `SPDX 2.2 JSON` | `-o spdx-json` |
| `SPDX 2.2 tag-value` | `-o spdx-tag-value` |
| `CycloneDX 1.5 JSON` | `-o cyclonedx-json` |
| `CycloneDX 1.5 XML` | `-o cyclonedx-xml` |

转换示例：
```shell
folib alpine:latest -o folib-json=sbom.folib.json # 生成 folib SBOM
folib convert sbom.folib.json -o cyclonedx-json=sbom.cdx.json  # 生成 CycloneDX
```

## 批量操作

上传命令支持上传文件夹，文件夹内部文件采用 **递归上传** 方式，示例如下：

```shell
./folib artifact upload -S 仓库ID -r 存储库ID -T "目标路径"  -f "文件夹路径"
```

**(1) 批量上传功能**

```shell
./folib artifact upload -b 'json字符串（需转义）'
```

| 参数 | 必选 | 参数解释 | 默认值 |
| :----: | :----: | :----: | :----: |
| `pattern` | ✅ | `glob` 路径匹配模式 | |
| `target` | ✅ | 文件上传到仓库的位置 | |
| `props` | | 元数据，格式 `key=value;key=value;` | |
| `recursive` | | 是否递归子目录，`true` 递归子目录，`false` 不递归子目录 | `true` |
| `flat` | | 是否创建子目录，`true` 创建子目录，`false` 不创建子目录 | `true` |

示例：

```shell
./folib artifact upload -b '{\"files\": [{ \"pattern\": \"D:\\\\document\\\\SBOM\\\\工作进展\\\\测试文件\\\\Js测试文件\\\\testFolder\\\\com*\", \"target\": \"test001/raw-test0",props\": \"jenkins.job=123;build_component=212\", \"recursive\": \"true\", \"flat\": \"true\" }] }'
```

**(2）批量下载功能**

```shell
./folib artifact download -b 'json字符串（需转义）'
```

| 参数 | 必选 | 参数解释 | 默认值 |
| :----: | :----: | :----: | :----: |
| `pattern` | ✅ | *glob* 路径匹配模式 | |
| `target` | ✅ | 文件下载到本地的位置 | |
| `recursive` | | 是否递归子目录，`true` 递归子目录，`false` 不递归子目录 | `true` |
| `flat` | | 是否创建子目录，`true` 创建子目录，`false` 不创建子目录 | `false` |

示例：

```shell
./folib artifact download -b '{\"files\":[{\"pattern\":\"aaa/generic/mariadb-*.tar\",\"target\":\"/Users/leipenghui/Downloads/\",\"recursive\":true,\"flat\":false}]}'
```