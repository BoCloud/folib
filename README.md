### 开发说明
##### 第一步  私有化maven仓库配置
确保folib-settings.xml中的本地仓库是你自己的存放路径
##### 第二步  环境需求
- 确保为了防止存在一些莫名奇妙的问题浪费时间，希望你个人电脑尽可能是linux内核或者unix内核的环境，通过IDEA进行开发。4核16G以上。该工程真的有点大，启动起来有点慢。
- maven 3.6.3 或以上版本。 JDK11,  nodejs14   yarn 1.22.x

#### 第三部  预编译
执行folib-package.sh 脚本 在工程的根目录下
```
#!/usr/bin/env bash
mvn clean --settings folib-settings.xml -Dmaven.test.skip=true
cd folib-web-vue
yarn install
yarn run build
cd ..
mvn  package --settings folib-settings.xml -Dmaven.test.skip=true
```
注意事项：
-  本项目为 all in one 前端vue+后端Springboot 但是打包时会整合到后端进行统一打包
- folib-web-vue是前端工程，该脚本会先将buid到 folib-web-core/src/main/resources目录下，然后进行整体工程打包。打包完成后方可本地进行启动。
- 启动类位置：
``` java
folib-web-core/src/main/java/com/veadan/folib/app/FolibSpringBootApplication.java
```
- application.yaml配置文件位置在folib-common模块下
```java
folib-commons/src/main/resources/application.yaml
```
- 启动后所有配置文件在folib文件夹下面，application.yaml中如果需要增加配置，需要将环境变量暴露出来

#### 打包发布
通过执行打包脚本，folib-distribution会自动将安装包和配置文件打包好，在：folib-distribution/target/目录下


#### 模块明细说明:

- [folib-aql](http://git.folib.com/folib/folib-server/src/branch/dev/folib-aql)
  这是aql查询语言模块，aql指的是通过常用的表达式进行搜索和查询的功能。

  该模块采用janusgraph之后当前不可用，后期要进行完善（优先级不高）。

- [folib-client](http://git.folib.com/folib/folib-server/src/branch/dev/folib-client)
  客户端模块封装了一些客户端调用的函数在里面。

- [folib-commons](http://git.folib.com/folib/folib-server/src/branch/dev/folib-commons)
  跨模块公共包，其中启动函数使用的application.yaml的配置文件在里面。

- [folib-configuration](http://git.folib.com/folib/folib-server/src/branch/dev/folib-configuration)
  该模块包含了配置解析功能的相关代码。

- [folib-cron](http://git.folib.com/folib/folib-server/src/branch/dev/folib-cron)

    - [folib-cron-api](http://git.folib.com/folib/folib-server/src/branch/dev/folib-cron/folib-cron-api)
      包含自定义Cron和控制器实现所需的Cron API代码。
    - [folib-cron-tasks](http://git.folib.com/folib/folib-server/src/branch/dev/folib-cron/folib-cron-tasks)
      包含常见的内置cron任务。

- [folib-data-service](http://git.folib.com/folib/folib-server/src/branch/dev/folib-data-service)
  包含数据服务类的基本实现。更多详细的内容请看folib-db代码工程

- [folib-distribution](http://git.folib.com/folib/folib-server/src/branch/dev/folib-distribution)
  此模块生成针对不同平台的最终发行版二进制文件。所有发布安装包打包通过该模块进行统一生成。

- [folib-event-api](http://git.folib.com/folib/folib-server/src/branch/dev/folib-event-api)
  此模块包含了事件的API，相关事件请参阅代码。

- [folib-rest-client](http://git.folib.com/folib/folib-server/src/branch/dev/folib-rest-client)
  包含REST API客户端。

- [folib-security](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security)

    - [folib-authentication-api](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-authentication-api)

    - [folib-authentication-providers](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-authentication-providers)

        - [folib-default-authentication-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-authentication-providers/folib-default-authentication-provider)

        - [folib-ldap-authentication-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-authentication-providers/folib-ldap-authentication-provider)

          ldap相关的实现在该模块中，后续要实现sso模块的对接

    - [folib-authentication-registry](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-authentication-registry)

    - [folib-authentication-support](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-authentication-support)

    - [folib-security-api](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-security-api)
      包含各种与安全性和加密相关的类。

    - [folib-user-management](http://git.folib.com/folib/folib-server/src/branch/dev/folib-security/folib-user-management)

      包含了用户管理相关的代码类。

- [folib-storage](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage)
  包含存储与不同制品库类型的相关模块的代码。

    - [folib-storage-api](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-api)
      包含存储相关API。

    - [folib-storage-core](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-core)
      包含存储API的核心类。

    - [folib-storage-layout-providers](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers)

        - [folib-storage-maven-layout](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-maven-layout)
            - [folib-maven-metadata-api](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-maven-layout/folib-maven-metadata-api)
              这是对`maven-metadata.xml`格式支持的实现。
            - [folib-storage-maven-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-maven-layout/folib-storage-maven-layout-provider)
              这是Maven布局提供程序的实现。它依赖于folib-maven-metadata-api。

    - [folib-storage-docker-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-docker-layout-provider)

      这是对容器Docker的布局进行实现，目前针对/v2/进行了实现。

    - [folib-storage-npm-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-npm-layout-provider)
      这是NPM布局提供程序的实现。它依赖于folib-npm-metadata项目。

    - [folib-storage-nuget-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-nuget-layout-provider)
      这是Nuget布局提供程序的实现。

    - [folib-storage-p2-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-p2-layout-provider)
      这是一个不完整的P2 OSGi布局提供程序的早期实现，可能会存在一些bug后期进行完善（TODO）。

    - [folib-storage-pypi-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-pypi-layout-provider)
      这是pypi布局提供程序的实现，还没有进行验证和测试（TODO）。

    - [folib-storage-raw-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-raw-layout-provider)
      这是Raw布局提供程序的实现。

    - [folib-storage-rpm-layout-provider](http://git.folib.com/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-rpm-layout-provider)
      这是RPM布局提供程序的实现,还存在bug还没有进行实现（TODO）。

- [folib-web-core](http://git.folib.com/folib/folib-server/src/branch/dev/folib-web-core)
  该模块是主函数启动类控制器以及前端api接口，其中引用了所有模块。在scanner包下拥有安全扫描相关的代码以及扫描报告与

- [folib-web-forms](http://git.folib.com/folib/folib-server/src/branch/dev/folib-web-forms)
  这个模块包含了所有的web表单所用到的实体对象，有的有用有的没用了。