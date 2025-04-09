# Nuget + Mono的示例

## 前置条件

`Monocli` 环境， [🔗 nuget.exe](https://www.nuget.org/downloads) ， `Microsoft.Build.dll` 如果运行`Mono pre version 3` 如果使用 `alias nuget="mono /usr/local/bin/nuget.exe"thenmono --runtime=v4.0 nuget.exe` 可以用 `nuget` 命令替换。

## NuGet示例配置

添加默认推送的 `folib` 制品仓库 `URL`

```shell
$ mono --runtime=v4.0 nuget.exe config -set DefaultPushSource={repositoryUrl} -ConfigFile ./.nuget/NuGet.config

#例如下面的命令(如果配置成功，则没有任何输出返回)：
$ mono --runtime=v4.0 nuget.exe config -set DefaultPushSource=http://localhost:38080/storages/folib-common/nuget-releases -ConfigFile ./.nuget/NuGet.config
```

`NuGet` 需要通过身份验证 `API Key` 才能部署或删除您的包。 `Folib` 提供 `REST API` 来获取指定用户的 `API Key` ，您可以使用 `curl` 如下方式：

```shell
$ curl -X GET --user admin:password http://localhost:38080/api/users/admin/generate-security-token

#返回如下
eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJTdHJvbmdib3giLCJqdGkiOiJCdU85OU8xV2VzQ1NkYWcyT3k0eHh3Iiwic3ViIjoiYWRtaW4iLCJzZWN1cml0eS10b2tlbi1rZXkiOiJhZG1pbi1zZWNyZXQifQ.Yzq5zYlDZVCVSxRmSgclRCHW_KojZw-iFGfkWWnTTEw
```

在mono中设置仓库的`API Key`:

```shell
$ mono --runtime=v4.0 nuget.exe setApiKey {apiKey} -Source {repositoryUrl} -ConfigFile ./.nuget/NuGet.config
```

示例操作与输出：

```shell
$ mono --runtime=v4.0 nuget.exe setApiKey bXktYXBpLWtleQ== -Source http://localhost:38080/storages/folib-common/nuget-releases -ConfigFile ./.nuget/NuGet.config

The API Key 'bXktYXBpLWtleQ==' was saved for 'http://localhost:38080/storages/folib-common/nuget-releases'.
```

如果需要在`NuGet.config`中配置凭据，则可以参考如下示例：

```shell
$ cat .nuget/NuGet.config 
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <config>
    <add key="DefaultPushSource" value="http://localhost:38080/storages/folib-common/nuget-releases" />
  </config>
  <apikeys>
    <add key="http://localhost:38080/storages/folib-common/nuget-releases" value="YpDSPr0yOqTjEPuaG6+aTOV6QJWI0X4MliV/yARLTZXb4cb55LaZF8jOhWvg+Zqnkn8ykhHtj3byEwKL60GWbsaeZZJdPHeP4OgFftPSmGkJSovyMRh1bbATPi6hx6eRpquP8daWKhfAvca0RjnPA22s3KtcdDlI3dV6IQzTLOfANkdmyhH95A+LHc51BXQKVWQPJ6B94TEBonEqWIt2bNti66Pd4sbDvKZJAA1GRjDprFxukg4EUz8YD++JYWP6X+BNCu2jYNXBS6tbw6Zx1o9HwOd/9eUC+1lP9Sbvj4tGSB/D5MwKhNabKwElhjikDNg5TaI4Il6R3sw9zJXyDdsGIfpKg4ICwBt6suuqEOQZQIWJKum3NuFYOocke6BsHpHC2Iz/hMkCjQz3v8DNaKLU+9pr6qOOaEsfyJCkj313AWxigkHqKcFMlJPfhGcUjZX6wq1vmPMO2erYBiE89IFCdAadBWpB2J6s79YoWwb5Elvf7SiLlU6lDEq9D8mOQLTeWrEkoD3S9h/CiV2qug==" />
  </apikeys>
  <packageSources>
    <add key="strongbox" value="http://localhost:38080/storages/folib-common/nuget-releases" />
  </packageSources>
  <packageSourceCredentials>
    <strongbox>
        <add key="Username" value="admin" />
        <add key="ClearTextPassword" value="password" />
    </strongbox>
  </packageSourceCredentials>
</configuration>
```

## 构建

将代码构建并创建一个 `dll` ,操作示例如下：

```shell
$ mcs -t:library -out:./bin/HelloWorld.dll ./src/HelloWorld.cs
```

## 创建构建NuGet包

命令如下：

```shell
$ mono --runtime=v4.0 nuget.exe pack ./Hello.Folib.Nuget.Mono.nuspec

#示例输出如下
Attempting to build package from 'Hello.Folib.Nuget.Mono.nuspec'.
Successfully created package 'Nuget.Mono.1.0.nupkg'.
```

## 如何将 NuGet 包推送到 Folib仓库

执行以下命令：

```shell
$ mono --runtime=v4.0 nuget.exe push ./Nuget.Mono.1.0.nupkg -ConfigFile ./.nuget/NuGet.config

#推送成功示例如下：
Pushing Nuget.Mono.1.0  to 'http://localhost:38080/storages/folib-common/nuget-releases'...
Your package was pushed.
```

## 如何在Folib仓库中搜索 NuGet包

命令如下：

```shell
$ mono --runtime=v4.0 nuget.exe list Com.Folib -ConfigFile ./.nuget/NuGet.config
```
