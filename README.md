<p align="center"><a href="https://folib.com"><img src="build/folib-logo.png" alt="Folib for AI" width="300" /></a></p>
<h3 align="center">一个为Ai研发而生的、全语言软件供应链服务平台</h3>
<p align="center">

<img src="https://img.shields.io/github/stars/vuejs/vue" alt="GitCode Stars">
<img src="https://img.shields.io/badge/dynamic/json?url=https://web-api.gitcode.com/api/v2/projects/Cangjie/Cangjie-Examples/simple&label=Stars&query=$.star_count&logo=gitpod&logoColor=red" alt="GitCode Stars">
<img src="https://img.shields.io/badge/Stars-1.2k-white?logo=gitpod&logoColor=red" alt="GitCode Stars">
<img src="https://img.shields.io/badge/Atomgit_Stars-6k-white" alt="Atomgit Stars">
<img src="https://img.shields.io/badge/release-v1.2.0-rgb(13,126,191)" alt="release">
<img src="https://img.shields.io/badge/springboot-v3.2.12-rgb(13,126,191)?logo=springboot" alt="springboot">
<img src="https://img.shields.io/badge/jdk-17-rgb(13,126,191)?logo=openjdk" alt="jdk">
<img src="https://img.shields.io/badge/license-GPL3.0-white">
<br>
</p>
<hr />

[English](./README_en.md) | 简体中文

FOLib 是一个为Ai研发而生的、全语言软件供应链服务平台。

-   **语言支持范围**：23+种全语言仓库，涵盖npm、Maven、PyPi、Docker、Gradle、SBT、Cocoapods、Swift、RPM、Debian、OPKG、PHP、Go、Pub、Ivy、NuGet、Conda、Cargo、Conan、Yarn、GitLFS、Helm、OHPM等主流工具；
-   **AI模型库与生态**：涵盖Huggingface、Ollama、ModelScope 等主流AI模型仓库的代理与同步， 并支持工具私有化上传与晋级分发；
-   **AIAgent与MCP支持**：支持元数据需求-服务-制品-安全漏洞-依赖证书等多维图数据的查询与展示，并支持MCP上下文协议，可通过AIAgent实现制品库的智能查询与推荐、安全漏洞的智能修复、智能晋级同步等功能；
-   **容器化与云原生支持**：支持Docker V1/V2/OCI镜像格式，支持nerdctl、crictl、ctr、podman等多客户端，支持分层传输，单层断点续传。 支持webdav为大文件提供云原生数据挂载能力。

## 快速开始

```
docker run -itd  --restart always --name folib -p 38080:38080 \
-p 7010:7010 -p 7011:7011 -p 7199:7199 -p 49142:49142 -p 8182:8182 \
-e FOLIB_MYSQL_HOST=mysql \
-e FOLIB_MYSQL_PORT=3306 \
-e FOLIB_MYSQL_DB=folib_scanner \
-e FOLIB_MYSQL_USER=root \
-e FOLIB_MYSQL_PASSWORD=folib@v587 \
-e FOLIB_PORT=38080 \
-v /home/folib/folib-conf:/opt/folib/folib-1.0-SNAPSHOT/etc/conf \
-v /home/folib/folib-vault:/opt/folib/folib-vault  \
docker.folib.com/folib-common/folib-docker/folib-server:1.0


# 用户名: admin
# 密码: folib@v587
```

你也可以通过 [HelmChat](https://artifacthub.io/packages/helm/folib/folib) 快速部署 Folib。

如果是内网环境，推荐使用 [离线安装包方式](https://public.folib.com) 进行安装部署。

如你有更多问题，可以通过论坛和技术交流群与我们交流。

-   [产品介绍与案例](https://folib.com/customers)

-   [演示环境](https://demo.folib.com)

### 技术交流群
<p align="left"><a href="https://folib.com"><img src="build/wecom.jpg" alt="Folib for AI" width="300" /></a></p>




## 版本说明

FOLib 按年发布 LTS（Long Term Support）版本。

- v3.00-lts：发布时间为 2025 年 8 月 1 日，持续更新中；


FOLib 产品版本分为社区版和企业版，详情请参见：[FOLib产品版本对比](https://folib.com/pricing)

## 技术栈

-   后端: [Spring Boot3.x](https://spring.io/projects/spring-boot)
-   前端: [Vue.js](https://vuejs.org/)
-   数据库: [MySQL](https://www.mysql.com/)
-   基础设施: [Docker](https://www.docker.com/)

## 开发编译说明
### 环境准备
-   安装 [OPENJDK 17](URL_ADDRESS-   安装 [OPENJDK 17](https://www.oracle.com/java/technologies
-   安装maven 3.8.6 
-   安装node 14.21.3
### 编译执行
在代码根路径下找到folib-package.sh文件并进行执行
```shell
  sh folib-package.sh
```


## License & Copyright

Folib - [新一代AI制品仓库]
Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。

This program is distributed WITHOUT ANY WARRANTY.
Commercial sale of this software is expressly prohibited.

For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
商业授权咨询请联系：folib@beyondcent.com
