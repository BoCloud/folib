# webdav 支持文档

## 文档概述

`Folib` 支持 `WebdAV` 。本文档是对 `webdav` 操作的示例，在示例中 `webdav` 的根目录是 `/dav` 。

## 功能介绍

+ **客户端访问**

以下载和安装 `syberduck` 为例：

通过客户端连接 `webdav`服务，测试目录展示复制、上传、下载、移动、删除等功能

:::tip
💡 仓库仅展示本地仓库
:::

![SCR-20250310-oxeq.png](https://www.huayanjun.cn/pics/blog-asset/2025/03/10/SCR-20250310-oxeq.png)

![SCR-20250310-oycc.png](https://www.huayanjun.cn/pics/blog-asset/2025/03/10/SCR-20250310-oycc.png)

+ *linux* **挂载**

	+ **安装** *davfs2*

	```sh
    sudo yum install epel-release -y

    sudo yum install davfs2
    ```

    + **创建挂载目录**

    ```sh
    sudo mkdir -p /mnt/webdav
    ```

    + **挂载时按要求输入账号名和密码**

	```sh
    sudo mount -t davfs http://10.50.9.37:38080/dav  /mnt/webdav
    ```

    + **进入** */mnt/webdav* **可以展示存储空间开始的目录结构**

	![SCR-20250310-oyvw.png](https://www.huayanjun.cn/pics/blog-asset/2025/03/10/SCR-20250310-oyvw.png)