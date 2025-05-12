# Helm仓库工具使用示例

## 一、Helm 环境安装
Helm 的安装方式有多种，以下以在 Linux 系统上使用脚本安装为例：
```bash 
curl -fsSL -o get_helm.sh https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 
chmod 700 get_helm.sh 
./get_helm.sh 
``` 
安装完成后，可通过 `helm version` 命令验证是否安装成功。

## 二、Helm 配置
1. 添加 folib Helm 仓到本地
   执行以下命令进行登录和添加仓库操作：
```bash 
helm registry login http://10.50.5.121/artifactory/api/helm/helm-local 
helm repo add helm-local http://10.50.5.121/artifactory/api/helm/helm-local 
``` 
执行 `helm repo list` 可查看已添加的仓库列表。

## 三、上传 Chart 包到 Helm 仓库
1. 安装 helm-cm-push 插件
   步骤 1：下载安装包
   从  下载适合你系统的 `helm-cm-push` 安装包。

步骤 2：解压安装包
将下载的安装包复制到 Helm 的 `plugins` 目录下并解压。可通过 `helm env` 命令查看 `plugins` 目录的位置。

2. 使用 helm-cm-push 命令上传
   步骤 1：进入插件 bin 目录
```bash 
假设 helm env 显示的 plugins 目录为 ~/.local/share/helm/plugins 
cd ~/.local/share/helm/plugins/helm-cm-push/bin 
``` 

步骤 2：执行上传操作
例如，上传 `/app/fluentd-4.5.2.tgz` 的 chart 包到 `helm-local` 仓库：
```bash 
./helm-cm-push /app/fluentd-4.5.2.tgz helm-local 
``` 
参数说明：
- 第一个参数：chart 包的全路径。
- 第二个参数：加入到本地的 Helm 仓库名。
- `--username` 和 `--password`：可选参数，用于鉴权。

## 四、Helm 常用命令
1. 更新本地仓库
```bash 
helm repo update 
``` 
该命令会更新本地缓存的仓库信息，确保能获取到最新的 chart 列表。

2. 搜索本地的 charts
   例如，搜索本地的 `mysql` charts：
```bash 
helm search repo mysql 
``` 

3. 下载 chart 到本地
   将最新的 `mysql` chart 下载到本地，可使用 `--version` 参数指定版本：
```bash 
helm pull helm-local/mysql --version 8.0.26 
```。 
