# Go示例

folib 制品库支持 Go 语言仓库和客户端使用

## 一、前提条件
- 确保您已经安装了 Go 语言环境。您可以从官方网站下载并安装 Go：。
- 确保您已经创建了一个 Go 项目，并在项目根目录下创建了一个 `go.mod` 文件。
- 确保您已经配置了全局代理，以便 Go 可以从 folib 制品库拉取依赖包。

## 二、配置 Go 全局代理
为了使用 folib 制品库作为 Go 的代理，你需要通过以下方式修改 Go 的全局代理：
```bash 
go env -w GOPROXY=https://127.10.10.2/artifactory/api/go/go 
``` 
执行此命令后，Go 将会从指定的代理地址拉取依赖包。注意，这里的代理地址 `https://127.10.10.2/artifactory/api/go/go` 即为 folib 制品库的 Go 仓库使用地址。

## 三、命令操作说明
1. 添加依赖
   使用 `go get` 命令可以为你的 Go 项目添加依赖。例如，如果你需要添加一个名为 `example.com/package` 的依赖，可以执行以下命令：
```bash 
go get example.com/package 
``` 
`go get` 会自动从配置的代理地址下载所需的依赖包。

2. 清除依赖缓存
   当你遇到依赖包问题或者需要更新依赖时，可以使用 `go clean -modcache` 命令清除依赖缓存。
```bash 
go clean -modcache 
``` 
执行此命令后，Go 会删除本地缓存的所有依赖包，下次使用 `go get` 或 `go mod download` 时会重新下载。

3. 下载依赖项
   `go mod download` 命令用于下载项目的所有依赖项。在项目根目录下执行此命令：
```bash 
go mod download 
``` 
该命令会根据项目的 `go.mod` 文件从配置的代理地址下载所需的依赖包。

## 四、仓库地址说明
folib 制品库的 Go 仓库使用地址为：
``` 
https://127.10.10.2/artifactory/api/go/go 
``` 
请确保在配置全局代理时使用此地址，以确保能够正确访问 folib 制品库的 Go 依赖包。

##五、常见问题及解决方法
1. 无法连接到代理地址
- 检查网络连接：确保你的网络正常，能够访问 `https://127.10.10.2`。
- 检查代理配置：确认 `go env -w GOPROXY` 命令执行成功，并且代理地址正确。

2. 依赖下载失败
- 清除缓存：使用 `go clean -modcache` 命令清除本地依赖缓存，然后重新执行 `go mod download`。
- 检查依赖版本：确保 `go.mod` 文件中指定的依赖版本在 folib 制品库中存在。

## 六、更多参考信息
关于 Go 语言的更多使用方法和命令说明，请参阅官方文档：。

