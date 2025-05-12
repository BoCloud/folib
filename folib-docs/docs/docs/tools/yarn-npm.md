# Yarn或NPM工具示例

您可以通过 `folib-examples` 工程中的 `hello-folib-npm` 工程进行测试与验证。

## 前置条件

* NodeJS 12 版本或更高版本

## .npmrc工程文件配置

首先，您需要配置 `npm` 使用 `Folib` 作为私有仓库使用。可以创建一个`.npmrc`文件在您的项目中。

通常创建您的 `.npmrc` 文件如下示例：

```shell
$ cat .npmrc
registry=http://demo.folib.com/storages/folib-npm/npm-releases
always-auth=true
email=demo@folib.com
_auth=YWRtaW46cGFzc3dvcmQ=
#说明
# _auth 是通过base64加密后用户名密码的token 

#你也可以使用用户名密码的方式配置如下
username=admin
password=password
```

## 上传部署命令

执行以下命令构建并部署到folib:

```shell
$ npm publish
#如果是yarn命令如下
$ yarn publish
```