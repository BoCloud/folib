# Pub 仓库工具使用示例

## 一、前置条件
1. 安装 Dart SDK
   要使用 Pub 仓库工具，首先需要安装 Dart SDK。Dart SDK 提供了 Dart 语言的开发环境，其中包含了 Pub 工具。可以根据不同的操作系统，按照以下方式进行安装：
- Windows：访问 ，下载 Windows 版本的安装包，然后按照安装向导完成安装。
- macOS：可以使用 Homebrew 进行安装，在终端中执行以下命令：
```bash 
brew tap dart-lang/dart 
brew install dart 
``` 
- Linux：不同的 Linux 发行版安装方式略有不同，以 Ubuntu 为例，可以通过以下命令添加 Dart 软件源并安装：
```bash 
sudo apt-get update 
sudo apt-get install apt-transport-https 
sudo sh -c 'wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -' 
sudo sh -c 'wget -qO- https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list' 
sudo apt-get update 
sudo apt-get install dart 
``` 

2. 验证安装
   安装完成后，在终端中执行以下命令验证 Dart SDK 是否安装成功：
```bash 
dart --version 
``` 
如果能正确显示 Dart 的版本信息，则说明安装成功。

## 二、配置 Pub 仓库
1. 修改 `pubspec.yaml` 文件
   在项目的根目录下找到 `pubspec.yaml` 文件，在其中添加 `publish_to` 字段，将其值设置为要使用的 Pub 仓库地址。例如：
```yaml 
name: your_project_name 
version: 1.0.0 
publish_to: https://127.10.10.2/artifactory/api/pub/pub-proxy 
``` 
2. 添加授权信息
   要向私有 Pub 仓库上传制品，需要获取并添加授权信息（token）。可以使用以下命令获取并添加 token：
```bash 
dart pub token add "https://127.10.10.2/artifactory/api/pub/pub-proxy" 
``` 
执行该命令后，会提示输入 token 信息，按照提示输入正确的 token 即可。

## 三、Pub 仓库工具常用操作示例
1. 添加依赖
   在项目中添加依赖可以使用 `dart pub add` 命令。例如，要添加 `http` 库作为依赖，可以执行以下命令：
```bash 
dart pub add http 
``` 
执行该命令后，`pubspec.yaml` 文件会自动更新，同时会下载并安装该依赖。

2. 清除依赖缓存
   如果依赖缓存出现问题，可以使用 `dart pub cache clean` 命令清除缓存。执行以下命令：
```bash 
dart pub cache clean 
``` 
清除缓存后，再次执行 `dart pub get` 命令会重新下载所有依赖。

3. 下载依赖项
   当项目的 `pubspec.yaml` 文件发生变化，或者需要下载新添加的依赖时，可以使用 `dart pub get` 命令。执行以下命令：
```bash 
dart pub get 
``` 
该命令会根据 `pubspec.yaml` 文件中的依赖信息，下载并安装所有依赖项。

4. 上传制品
   当项目开发完成，需要将制品上传到 Pub 仓库时，可以使用 `dart pub publish` 命令。执行以下命令：
```bash 
dart pub publish 
``` 
执行该命令前，请确保已经正确配置了 `pubspec.yaml` 文件中的 `publish_to` 字段，并且添加了正确的授权信息。

## 四、参考文档
更多关于 Pub 工具的详细使用方法，可以参阅 。

