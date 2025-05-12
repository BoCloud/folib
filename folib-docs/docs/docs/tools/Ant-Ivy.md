# Ant + Ivy的示例

本示例用来说明 `Ant` 工具结合 `Ivy` 类型仓库进行打包下载,示例代码在: `/folib-examples/hello-folib-ant-ivy` 。

## 前置条件

* [🔗 Ant1](https://ant.apache.org/) .9.14 或更高版本
* [🔗 Ivy](https://ant.apache.org/ivy/)

## Ivy配置

在这之前请确保以下 `Ivy` 环境变量已经配置好：

对 `build.xml` 文件进行配置，这是针对编译代码、生成 `jar` 制品（部署）到 `Folib` 仓库的 `Ant` 构建脚本。 `ivysettings.xml`文件需要配置以下内容： `- resolvers`：拉取制品的仓库 `- publications`：目标部署的仓库 `ivy.xml`文件为依赖文件，制品包通过定义，依赖包是通过定义 `credentials.properties`文件， `Folib` 的访问凭证配置在该文件中。一般在您的`~/.ivy`隐藏目录下

## vy部署上传

执行以下命令构建并部署到 `Folib` :

```shell
ant build deploy
```
