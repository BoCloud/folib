<p align="center"><a href="https://folib.com"><img src="build/folib-logo.png" alt="Folib for AI" width="300" /></a></p>
<h3 align="center">A Full-Language Software Supply Chain Service Platform Built for AI R&D</h3>
<p align="center">

<img src="https://img.shields.io/github/stars/vuejs/vue" alt="GitCode Stars">
<img src="https://img.shields.io/badge/dynamic/json?url=https://web-api.gitcode.com/api/v2/projects/Cangjie/Cangjie-Examples/simple&label=Stars&query=$.star_count&logo=gitpod&logoColor=red" alt="GitCode Stars">
<img src="https://img.shields.io/badge/Stars-1.2k-white?logo=gitpod&logoColor=red" alt="GitCode Stars">
<img src="https://img.shields.io/badge/Atomgit_Stars-6k-white" alt="Atomgit Stars">
<img src="https://img.shields.io/badge/release-v1.2.0-rgb(13,126,191)" alt="release">
<img src="https://img.shields.io/badge/springboot-v3.2.12-rgb(13,126,191)?logo=springboot" alt="springboot">
<img src="https://img.shields.io/badge/jdk-17-rgb(13,126,191)?logo=openjdk" alt="jdk">
<br>
</p>
<hr />

[简体中文](./README.md) | English

FOLib is a full-language software supply chain service platform built for AI R&D.

- **Language Support**: 23+ full-language repositories, covering mainstream tools such as npm, Maven, PyPi, Docker, Gradle, SBT, Cocoapods, Swift, RPM, Debian, OPKG, PHP, Go, Pub, Ivy, NuGet, Conda, Cargo, Conan, Yarn, GitLFS, Helm, OHPM, etc.;
- **AI Model Library & Ecosystem**: Covers proxy and synchronization of mainstream AI model repositories such as Huggingface, Ollama, ModelScope, and supports private upload and promoted distribution of tools;
- **AIAgent & MCP Support**: Supports query and display of multi-dimensional graph data such as metadata requirements-services-artifacts-security vulnerabilities-dependency certificates, and supports MCP context protocol. It can realize intelligent query and recommendation of artifact libraries, intelligent repair of security vulnerabilities, intelligent promotion and synchronization through AIAgent;
- **Containerization & Cloud-Native Support**: Supports Docker V1/V2/OCI image formats, supports multiple clients such as nerdctl, crictl, ctr, podman, supports layered transmission, and single-layer resumable transmission. Supports webdav to provide cloud-native data mounting capabilities for large files.

## Quick Start

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


# Username: admin
# Password: folib@v587
```

You can also quickly deploy Folib via [HelmChat](https://artifacthub.io/packages/helm/folib/folib).

For intranet environments, it is recommended to use the [offline installation package](https://public.folib.com) for installation and deployment.

If you have more questions, you can communicate with us through the forum and technical exchange group.

- [Product Introduction & Cases](https://folib.com/customers)

- [Demo Environment](https://demo.folib.com)

### Technical Exchange Group
<p align="left"><a href="https://folib.com"><img src="build/wecom.jpg" alt="Folib for AI" width="300" /></a></p>




## Version Description

FOLib releases LTS (Long Term Support) versions annually.

- v3.00-lts: Release date is August 1, 2025, under continuous update;


FOLib product versions are divided into community edition and enterprise edition. For details, please refer to: [FOLib Product Version Comparison](https://folib.com/pricing)

## Development & Compilation Instructions
### Environment Preparation
- Install [OPENJDK 17](https://www.oracle.com/java/technologies
- Install maven 3.8.6
- Install node 14.21.3
### Compilation & Execution
Find the folib-package.sh file in the root directory of the code and execute it
```shell
  sh folib-package.sh
```


## License & Copyright

Folib - [Next-Generation AI Artifact Repository]
Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is free software: you can redistribute and/or modify it under the terms of the GNU General Public License (GPL-3.0+), but any form of commercial sale is prohibited (including but not limited to: direct sales, bundled sales, cloud service commercial use).

This program is distributed WITHOUT ANY WARRANTY.
Commercial sale of this software is expressly prohibited.

For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
For commercial authorization consultation, please contact: folib@beyondcent.com
