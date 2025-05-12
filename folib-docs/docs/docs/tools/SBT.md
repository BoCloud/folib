# SBT的示例

本示例源码在 `folib-examples/hello-folib-sbt` 工程中。将会介绍将 `SBT` 如何使用 `Folib` 仓库。

## 前置条件

[🔗 SBT 1.3](https://www.scala-sbt.org/) 版本或者更高

## SBT配置

`build.sbt` 文件用于定义项目的相关信息，例如依赖项、用于解析和部署工件的存储库、要使用的插件等。

需要具备以下参数，这里进行介绍与说明：

```shell
organization                      - 项目前缀。
name                              - 制品的名称。
version                           - 制品的版本。
publishMavenStyle := true         - 用于指定远程仓库的布局格式是基于 Maven 的。
credentials                       - 定义远程存储库的凭据。
resolvers                         - 定义从哪里解决依赖关系。
publishTo                         - 定义将此构建生成的制品部署到何处。
```

`repositories` 此文件列出了解析制品时要使用的远程仓库（及其布局）

凭据可以通过环境变量的方式进行定义，例如： `-Dfolib.username=maven -Dfolib.password=password` , `build.sbt` 能够识别。

## SBT部署上传包

执行以下命令构建并上传部署：

```shell
sbt compile publish
```