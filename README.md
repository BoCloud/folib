### 开发说明
##### 第一步  私有化maven仓库配置
确保folib-settings.xml中的本地仓库是你自己的存放路径
##### 第二步  环境需求
确保maven 3.6.3 或以上版本。 JDK11,  nodejs14   yarn 1.22.x

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


