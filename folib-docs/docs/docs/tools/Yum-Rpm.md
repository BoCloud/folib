# Yum+Rpm的示例

该示例将介绍 `yum` 源如何配置 `folib` 中的 `Rpm` 仓库，当然您已可以直接在仓库的使用 **说明按钮** 中进行查看。

## RPM配置

在 `/etc/yum.repos.d/` 中添加一个 `xxx.repo` 文件，以代理镜像服务，操作如下:

```shell
vim xxx.repo

[local_test]
name=CentOS-$releasever - Base - mirrors.aliyun.com
enabled=1
baseurl=http://localhost:38080/storages/taibao/163-rpm/ #folib仓地址
gpgcheck=0
```

## 命令操作

添加之后可以选择性操作以下命令：

```shell
yum clean all #清除YUM缓存
yum repolist #显示所有仓库
yum install --downloadonly --downloaddir=/folib_test/mysql mysql #拉mysql 相关rpm包到/folib_test/mysql 目录下
```