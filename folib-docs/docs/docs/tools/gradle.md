# Gradle工具示例

本示例将介绍，Folib下 [🔗 Gradle](https://gradle.org/) 仓库的使用，示例代码在： `/folib-examples/hello-folib-gradle`

## 前置条件

* [🔗 Gradle](https://gradle.org/) 2.11 或更高版本

## Gradle工程配置介绍

`gradle.build` 文件，是一个用 `Groovy` 编写的构建脚本。可以通过以下方式定义和定制与制品相关的属性：

| 属性 | 说明 |
|----|----|
|  `group`  |  指定 逻辑前缀（如groupId在 Mavenpom.xml文件中）  |
|  `version`  |  指定制品的版本  |
|  `repositories`  |  部分用来添加制品库，例如 Maven Central（默认包含）  |
|  `dependencies`  |  类似maven中的依赖，可以访问gradle仓库拉取依赖， [🔗 更多内容](https://docs.gradle.org/current/userguide/plugins.html)  |
|  `uploadArchives`  |  指定上传部署的仓库，需要和 Maven `distributionManagement`保持一致。  |

了解更多请阅读 [🔗 gradle 文档](https://docs.gradle.org/current/userguide/tutorial_using_tasks.html)。

`settings.gradle` 文件配置用来在 `gradle build` 的项目中，通过 `rootProject.name` 参数指定制品名称。

- **用户名密码凭证配置**

`gradle.properties` 凭据配置文件，一般放在项目根目录或 `~/.gradle` 目录(全局配置)。配置如下：

```shell
mavenUser=maven
mavenPassword=password
```

可以通过jvm参数进行指定，例如：

```shell
-Dcredentials.username=maven -Dcredentials.password=password
```

- **构建上传**

可以在您的 `Gradle` 项目工程中执行以下命令，将工程打包并上传到仓库(前提是已经将仓库配置指定好了)：

```shell
gradle clean upload
```

- **其他Gradle的使用文档可共参考**

[🔗 Gradle：制品管理](https://docs.gradle.org/current/userguide/artifact_management.html)

[🔗 Gradle：构建环境](https://docs.gradle.org/current/userguide/build_environment.html)

[🔗 Spring：使用 Gradle 构建 Java 项目](https://spring.io/guides/gs/gradle/)
