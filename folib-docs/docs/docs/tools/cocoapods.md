# Cocoapods工具示例

Cocoapods仓库通常用于管理 `Mac Swift` 等相关开发的依赖

了解更多请阅读 [🔗 Cocoapods](https://guides.cocoapods.org/) 文档

## 前置条件

* 确保按照Cocoapods工具
* 通过gem install cocoapods-art进行安装"cocoapods-art"插件

## 基础用法

<div class="custom-divider">
  <span class="divider-inner">了解如何配置cocoapods仓库</span>
</div> 

repo-art使用标准netrc file中指定的身份验证。

首先可以通过以下命令来配置你的身份验证：

```shell
machine demo.folib.com
login <USERNAME>
password <PASSWORD>
```

<div class="custom-divider">
  <span class="divider-inner">如何快速添加仓库和使用</span>
</div> 

这里将列出常用的添加仓库的命令：

```shell
pod repo-art add swift-local "https://demo.folib.com/storages/folib-common/swift-local"

#要从您添加的 Artifactory 规范存储库中解析 Pod，您必须将以下内容添加到 Podfile 中：
plugin  'cocoapods-art', :sources => [
  'swift-local'
]
```

如果正常拉取依赖则使用`pod install`命令操作
