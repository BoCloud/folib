# Pypi的示例

## 前置条件

[🔗 Python3](https://www.python.org/)版本或者更高
[🔗 twine](https://pypi.org/project/twine/)
[🔗 pip](https://pypi.org/project/pip/)

## 示例配置

本示例源码在 `folib-examples/hello-folib-pypi` 工程。 `setup.py` 该文件是 `setuptools` 。 它用来定义名称、描述、当前版本等信息。

`setup.cfg` 文件用来定义描述文件名等元数据信息。

`LICENCE文件` ，用于定义许可证详细信息。

## 构建python包

在您的项目文件夹中执行以下命令：

```shell
python3 setup.py sdist bdist_wheel
```

## 部署python包操作

将 `python` 包部署到 `Folib` ,命令如下：

```shell
python3 -m twine upload --username admin --password password --repository-url http://localhost:38080/storages/folib-common/pypi-releases  dist/* --verbose
```

## 下载与安装python包操作

安装 `python` 包，执行以下命令：

```shell
pip3 install --extra-index-url http://localhost:38080/storages/folib-common/pypi-releases hello-folib-pypi
```