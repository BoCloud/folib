# Nuget + Chocolatey的示例

安装Chocolatey Chocolatey 在 Windows 上原生可用，可以按照 [🔗 官方文档进行安装](https://chocolatey.org/install)

* Linux/MacOS

如果您的操作系统是 Linux/MacOS 并使用 [🔗 mono](https://www.mono-project.com/)，但没有事先构建好的二进制文件。需要前往 [🔗 Chocolatey仓库](https://github.com/chocolatey/choco)查看构建说明。

## 【配置Choco】获取 API 密钥

`NuGet` 用户需要通过身份验证 `API Key` 才能部署或删除您的包。我们提供 `API` 来获取指定用户的 `API Key` ：

``` shell
API_KEY=`curl -u admin http://localhost:38080/api/users/admin/generate-security-token`
echo $API_KEY
```

然后输入您的密码，随后 `echo` 输出来的类似如下的秘钥，不为空则表示成功：

```shell
$ eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJTdHJvbmdib3giLCJqdGkiOiJtU3N0TGVOMGpabzJNcmdleGdWSUVRIiwic3ViIjoiYWRtaW4iLCJzZWN1cml0eS10b2tlbi1rZXkiOiJhZG1pbi1zZWNyAMQifQ.SgpKb4yUidK8ATbGxDOfjGjHfEF22PIFyzlpk-Rpad0
```

## 【配置Choco】在 choco 中保存 API 密钥

设置 `apikey` ， `source` 在接下来的步骤中会在身份验证的时候使用到 `apikey` 。

```shell
REPO_URL=http://localhost:38080/storages/folib-common/nuget-releases
$ choco apikey -k $API_KEY -s "$REPO_URL"
  
#如果保存成功则会输出如下：
  
Chocolatey v.10.15
Added ApiKey for http://localhost:38080/storages/folib-common/nuget-releases
```

## 【配置Choco】将仓库添加到 Chocolatey 包源

为了让 `Chocolatey` 可以以访问您的仓库，你需要执行以下的命令进行配置：

```shell
$ choco source add -n=folib -s "$REPO_URL" --priority=1
       
#如果添加成功则输出如下
Chocolatey v0.10.15
Added folib - http://localhost:38080/storages/folib-common/nuget-releases (Priority 1)
```

:::warning 提醒
请注意，组合库类型的 Nuget 是不允许上传的，你只能上传到本地类型的 Nuget 仓库
:::

## 部署到Nuget仓库

在目录中执行以下命令：

```shell
$ choco push --source "$REPO_URL" --force
```

## Nuget仓库中搜索包

在目录中执行以下命令：

```shell
$ choco search -s "$REPO_URL"
```
