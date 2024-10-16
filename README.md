 ### [详情文档请看WIKI](http://58.210.154.140:8888/folib/folib-server/-/wikis/home)

- [新手上手，看看能否跑起来](http://58.210.154.140:8888/folib/folib-server/-/wikis/%E6%96%B0%E6%89%8B%E4%B8%8A%E6%89%8B)
- [进一步了解，看看各个模块拥有哪些功能](http://58.210.154.140:8888/folib/folib-server/-/wikis/%E8%BF%9B%E4%B8%80%E6%AD%A5%E4%BA%86%E8%A7%A3)
- [动手开发并了解各种布局和详细](http://58.210.154.140:8888/folib/folib-server/-/wikis/%E8%BF%9B%E9%98%B6%E7%BA%A7%E5%AD%A6%E4%B9%A0)

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
- IDEA启动注意事项：如果
#### 打包发布
- 通过执行打包脚本，folib-distribution会自动将安装包和配置文件打包好，在：folib-distribution/target/目录下

- 可以通过 unzip解压folib-distribution-1.0-SNAPSHOT.zip 后存在如下啊文件夹内容：
```shell
├── Dockerfile
├── folib-1.0-SNAPSHOT
└── folib-vault
```
- 在该目录下利用Dockerfile进行build
```shell
docker build -t folib-1.0 .
 docker buildx build --platform linux/amd64 -t folib-server:1.2.3.12-dev .
```
- docker启动
```shell
docker run -itd  --restart always --name folib -p 38081:38081 \
-e FOLIB_MYSQL_HOST=192.168.5.166 \
-e FOLIB_MYSQL_PORT=3306 \
-e FOLIB_MYSQL_DB=folib \
-e FOLIB_MYSQL_USER=root \
-e FOLIB_MYSQL_PASSWORD=folib \
-e FOLIB_ES_HOST=192.168.5.166 \
-e FOLIB_PORT=38081 \
folib-1.0:latest
```
- 环镜变量描述

| 变量名称                     | 含义                               | 默认值        | 是否必填 | 使用场景            |
| ---------------------------- | ---------------------------------- | ------------- | -------- | ------------------- |
| FOLIB_PORT                   | 主程序端口号                       | 38080         | 是       |                     |
| FOLIB_JVM_XMX                | 最大JVM内存                        | 512           | 是       |                     |
| FOLIB_DB_PROFILE             | db模式                             | db_EMBEDDED   | 是       |                     |
| FOLIB_DISTRIBUTED_LOCKIP     | 集群模式下对外其他节点暴露自身的IP | 无            |          |                     |
| FOLIB_CLUSTER_OPENFLAG       | 是否开启集群模式                   | false         | 是       |                     |
| FOLIB_CLUSTER_HOSTNODE       | 其他集群节点地址逗号隔开           | 无            |          |                     |
| FOLIB_GREMLIN_SERVER_ENABLED | GREMLIN图数据服务是否对外开启      | false         | 是       |                     |
| FOLIB_LOG_FILE_ENABLED       | 日志文件开启                       | true          | 是       |                     |
| FOLIB_LOG_FILE_SIZE_SINGLE   | 日志单个大小                       | 128MB         | 是       |                     |
| FOLIB_LOG_FILE_SIZE_TOTAL    | 日志文件总大小                     | 1GB           | 是       |                     |
| FOLIB_LOG_FILE_HISTORY       | 历史存储数量                       | 31            | 是       |                     |
| FOLIB_ES_HOST                | 索引ES存储地址                     | 10.50.8.55    | 是       |                     |
| FOLIB_MYSQL_HOST             | MySQL地址                          | 10.50.8.55    | 是       |                     |
| FOLIB_MYSQL_PORT             | MySQL端口                          | 3306          | 是       |                     |
| FOLIB_MYSQL_DB               | MySQL数据库名称                    | folib_scanner | 是       |                     |
| FOLIB_MYSQL_USER             | MySQL数据库账号                    |               | 是       |                     |
| FOLIB_MYSQL_PASSWORD         | MySQL数据库密码                    |               | 是       |                     |
| FOLIB_NVD                    | 安全策略镜像地址                   | nvd.folib.com/feeds/json/cve/1.1 | 否       |                     |
| FOLIB_JMX_PORT               | JMX监控端口                        | 7199          | 是       |                     |
| FOLIB_REMOTE_DB_HOST         | 外置图数据库持久化地址             | 127.0.0.1     | 否       | db_REMOTE模式下有效 |
| FOLIB_REMOTE_DB_PORT         | 外置图数据库持久化端口             | 49142         | 否       | db_REMOTE模式下有效 |
| FOLIB_REMOTE_DB_USER         | 外置图数据库用户名                 | cassandra     | 否       | db_REMOTE模式下有效 |
| FOLIB_REMOTE_DB_PASS         | 外置图数据持久化密码               | cassandra     | 否       | db_REMOTE模式下有效 |
#### 模块明细说明:

- [folib-aql](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-aql)
  这是aql查询语言模块，aql指的是通过常用的表达式进行搜索和查询的功能。

  该模块采用janusgraph之后当前不可用，后期要进行完善（优先级不高）。

- [folib-client](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-client)
  客户端模块封装了一些客户端调用的函数在里面。

- [folib-commons](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-commons)
  跨模块公共包，其中启动函数使用的application.yaml的配置文件在里面。

- [folib-configuration](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-configuration)
  该模块包含了配置解析功能的相关代码。

- [folib-cron](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-cron)

    - [folib-cron-api](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-cron/folib-cron-api)
      包含自定义Cron和控制器实现所需的Cron API代码。
    - [folib-cron-tasks](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-cron/folib-cron-tasks)
      包含常见的内置cron任务。

- [folib-data-service](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-data-service)
  包含数据服务类的基本实现。更多详细的内容请看folib-db代码工程

- [folib-distribution](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-distribution)
  此模块生成针对不同平台的最终发行版二进制文件。所有发布安装包打包通过该模块进行统一生成。

- [folib-event-api](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-event-api)
  此模块包含了事件的API，相关事件请参阅代码。

- [folib-rest-client](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-rest-client)
  包含REST API客户端。

- [folib-security](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security)

    - [folib-authentication-api](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-authentication-api)

    - [folib-authentication-providers](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-authentication-providers)

        - [folib-default-authentication-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-authentication-providers/folib-default-authentication-provider)

        - [folib-ldap-authentication-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-authentication-providers/folib-ldap-authentication-provider)

          ldap相关的实现在该模块中，后续要实现sso模块的对接

    - [folib-authentication-registry](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-authentication-registry)

    - [folib-authentication-support](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-authentication-support)

    - [folib-security-api](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-security-api)
      包含各种与安全性和加密相关的类。

    - [folib-user-management](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-security/folib-user-management)

      包含了用户管理相关的代码类。

- [folib-storage](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage)
  包含存储与不同制品库类型的相关模块的代码。

    - [folib-storage-api](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-api)
      包含存储相关API。

    - [folib-storage-core](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-core)
      包含存储API的核心类。

    - [folib-storage-layout-providers](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers)

        - [folib-storage-maven-layout](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-maven-layout)
            - [folib-maven-metadata-api](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-maven-layout/folib-maven-metadata-api)
              这是对`maven-metadata.xml`格式支持的实现。
            - [folib-storage-maven-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-maven-layout/folib-storage-maven-layout-provider)
              这是Maven布局提供程序的实现。它依赖于folib-maven-metadata-api。

    - [folib-storage-docker-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-docker-layout-provider)

      这是对容器Docker的布局进行实现，目前针对/v2/进行了实现。

    - [folib-storage-npm-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-npm-layout-provider)
      这是NPM布局提供程序的实现。它依赖于folib-npm-metadata项目。

    - [folib-storage-nuget-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-nuget-layout-provider)
      这是Nuget布局提供程序的实现。

    - [folib-storage-p2-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-p2-layout-provider)
      这是一个不完整的P2 OSGi布局提供程序的早期实现，可能会存在一些bug后期进行完善（TODO）。

    - [folib-storage-pypi-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-pypi-layout-provider)
      这是pypi布局提供程序的实现，还没有进行验证和测试（TODO）。

    - [folib-storage-raw-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-raw-layout-provider)
      这是Raw布局提供程序的实现。

    - [folib-storage-rpm-layout-provider](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-storage/folib-storage-layout-providers/folib-storage-rpm-layout-provider)
      这是RPM布局提供程序的实现,还存在bug还没有进行实现（TODO）。

- [folib-web-core](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-web-core)
  该模块是主函数启动类控制器以及前端api接口，其中引用了所有模块。在scanner包下拥有安全扫描相关的代码以及扫描报告与

- [folib-web-forms](http://58.210.154.140:8888/folib/folib-server/src/branch/dev/folib-web-forms)
  这个模块包含了所有的web表单所用到的实体对象，有的有用有的没用了。


### 事件监听机制
#### 事件
事件的扩展基于com.veadan.folib.event.Event 类

#### 事件监听器注册表
事件侦听器实例必须在相应实现的侦听器注册表中注册，该注册表将用于向它们分派事件。

所有事件侦听器都必须扩展com.veadan.folib.event.AbstractEventListenerRegistry基类。

考虑以下示例，说明如何注册您的侦听器(具体参考实际已有的代码)：
```java
public class ArtifactEventHandlingExample
{

    @Inject
    ArtifactEventListenerRegistry artifactEventListenerRegistry;

    public void doStuff()
    {
        // Create the listener
        DummyArtifactEventListener listener = new DummyArtifactEventListener();

        // Add the listener to the registry
        artifactEventListenerRegistry.addListener(listener);

        // Create an event
        ArtifactEvent artifactEvent = new ArtifactEvent(ArtifactEvent.EVENT_ARTIFACT_UPLOADED);

        // Tell the registry to dispatch the event to all registered listeners:
        artifactEventListenerRegistry.dispatchEvent(artifactEvent);
    }

    private class DummyArtifactEventListener implements ArtifactEventListener
    {

        @Override
        public void handle(ArtifactEvent event)
        {
            System.out.println("Caught artifact event type " + event.getType() + ".");
        }

    }

}
```
### 事件监听机制
#### 事件
事件的扩展基于com.veadan.folib.event.Event 类，主要用来制品包仓库创建等上传下载，事件分发和监听的实现，例如：制品上传后需要进行安全扫描等场景。

#### 事件监听器注册表
事件侦听器实例必须在相应实现的侦听器注册表中注册，该注册表将用于向它们分派事件。

所有事件侦听器都必须扩展com.veadan.folib.event.AbstractEventListenerRegistry基类。

考虑以下示例，说明如何注册您的侦听器(具体参考实际已有的代码)：
```java
public class ArtifactEventHandlingExample
{

    @Inject
    ArtifactEventListenerRegistry artifactEventListenerRegistry;

    public void doStuff()
    {
        // Create the listener
        DummyArtifactEventListener listener = new DummyArtifactEventListener();

        // Add the listener to the registry
        artifactEventListenerRegistry.addListener(listener);

        // Create an event
        ArtifactEvent artifactEvent = new ArtifactEvent(ArtifactEvent.EVENT_ARTIFACT_UPLOADED);

        // Tell the registry to dispatch the event to all registered listeners:
        artifactEventListenerRegistry.dispatchEvent(artifactEvent);
    }

    private class DummyArtifactEventListener implements ArtifactEventListener
    {

        @Override
        public void handle(ArtifactEvent event)
        {
            System.out.println("Caught artifact event type " + event.getType() + ".");
        }

    }

}
```
### 架构说明
#### 布局关系说明
下图是 Storages, Repositories 和 Layout Providers之间的关系，如果你要扩展其他工具布局需要了解。
![folib-layout](uploads/a6cfcdb5e53382ca78b97fbf4f1850bc/folib-layout.png)

##### Repository 仓库
- Hosted 本地模式
- Proxy 代理模式
- Group 组合模式

##### Layout 布局
- Maven
- NPM
- NuGet
- Raw
- Docker
- 等其他布局

##### Storage 存储
- File存储模式 （NFS已经支持）
- AWS S3 (对象存储，将来需要支持)

所有层都是松散耦合的，实现上并不相互依赖。

#### 布局实现逻辑
制品artifacts只是普通的文件，我们的实现主要是基于JDK File I/O（Featuring NIO.2）实体。
![class](uploads/a1d05b19a8b8682b2eb5ef94163a5d01/class.png)

##### 需要实现的类
- ConcreteLayoutFileSystemProvider
- ConcreteLayoutFileSystem
- LayoutProvider
- ArtifactCoordinates
  具体参考现有代码，可能不太对。

#### Artifact Coordinates
##### ArtifactCoordinates.java实现的要求
- 每个ArtifactCoordinates实现都应该有一个id、 version。
- 每个id和version对每个存储库都必须是唯一的。
- 应该有一个传递函数，ArtifactCoordinates反之亦然Path。
##### 每个布局实现都应该放在模块下的单独模块
```java
folib-storage/folib-storage-layout-providers 下
```
#### Artifact 接口控制器实现
ArtifactCoorsinates默认支持一些API。大多数工具都使用 HTTP 进行交互，在 Folib 中，使用Spring MVC实现。BaseArtifactController.java支持默认的 API 方法（下载、上传等），可进行扩展。

#### 特定布局I/O实现和扩展
要使布局实现真正可用，有时候存在一些特定的 I/O，例如 Streams ( InputStream, OutputStream) 和文件系统相关实体 ( FileSystemProvider, FileSystem, LayoutProvider)。
##### 需要扩展以下：
- LayoutFileSystem
- LayoutFileSystemProvider
- AbstractLayoutProvider
##### 另外几乎所有的包括Layout相关的组件都由 Spring 的 IoC 容器管理，以下工厂类需要放在上下文中：
- LayoutFileSystemProviderFactory
- LayoutFileSystemFactory

#### 流程时序图
![flow](uploads/dfff7b25cbbb9668adb4602b8587475e/flow.png)

## 权角色限说明
ADMIN角色：
- 描述：管理员，拥有所有权限
- 限制：不可删除（isDefault=1）,不可选择资源

ANONYMOUS：
- 描述：匿名用户，未登录用户
- 限制：此角色不可删除（isDefault=1）,不能添加用户、用户组，权限直接和角色或指定资源关联，可选择资源

GENERAL：
- 描述：普通用户，拥有部分权限，拥有除下载以外的所有api权限
- 限制：不可删除（isDefault=1）,不可选择资源

OPEN_SOURCE_MANAGE：
- 描述：开源项目管理员，拥有开源项目管理权限
- 限制：不可删除（isDefault=1）,不可选择资源

READERS：
- 描述：只读用户，拥有只读权限，只读权限包含：下载、查询
- 限制：不可删除（isDefault=1）,不可选择资源

STORAGE_ADMIN_开头角色：
- 描述：存储空间管理员，创建存储空间时同步创建
- 限制：只能选择一个用户作为管理员，不可选择资源、权限

STORAGE_USER_开头角色：
- 描述：存储空间普通用户角色，创建存储空间时同步创建
- 限制：不可选择资源
