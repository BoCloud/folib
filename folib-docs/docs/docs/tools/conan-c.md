# Conan工具示例

可以通过 `folib-examples/hello-folib-conan` 工程或者你的随便一个 `c/c++` 工程工程进行测试与验证，目的是用来体验如何通过 `Conan` 相关命令工具与制品库进行交互。

## 前置条件

* Conan版本 1.6.x版本
* 可通过pip install conan进行安装

## 基础用法

<div class="custom-divider">
  <span class="divider-inner">了解如何配置remote仓库</span>
</div> 

`conan remote list` 用于查询你本地 `Conan` 工具配置的远程仓库的列表。

首先可以通过以下命令来配置你的仓库：

```bash
conan remote add conan-proxy https://demo.folib.com/folib-common/conan-proxy false

conan user -p [password] -r a_local_conan [username]   #添加访问用户名密码
```

<div class="custom-divider">
  <span class="divider-inner">如何快速搜索远程以及上传</span>
</div> 

这里将列出常用的Conan命令：

```shell
1.   搜索本地已有的Conan

conan  search
2.   上传本地包到   conan-proxy

例如上传 zulu-openjdk/11.0.15
conan  upload  zulu-openjdk/11.0.15@ -r conan-proxy  --all

3.   下载与搜索   conan-proxy

例如下载 conan-proxy  zulu-openjdk/11.0.15

conan   search    zulu-openjdk -r   conan-proxy
conan   download   zulu-openjdk/11.0.15@    -r   conan-proxy
```

了解更多请阅读 [🔗 Conan](https://docs.conan.io/1/reference/commands.html) 文档