# Docker 仓库配置示例

本示例详细介绍了不同操作系统下如何配置 Docker 仓库，同时也会涵盖一些其他开源容器客户端（nerdctl、crictl、ctr、podman）的配置方法。若您需要，也可直接在仓库的使用说明按钮中查看相关信息。

## 前提条件
本文档适用于 Docker 客户端版本大于 1.10.0 的用户，不同操作系统的配置步骤有所差异。

## 各操作系统 Docker 仓库配置

### Ubuntu 配置
1. 创建 `/etc/docker` 目录（若不存在）：
```shell 
sudo mkdir -p /etc/docker 
``` 
2. 编辑 `daemon` 配置文件 `/etc/docker/daemon.json`：
```shell 
sudo tee /etc/docker/daemon.json <<-'EOF' 
{ 
    "insecure-registries": ["ip地址:38080"] 
} 
EOF 
``` 
3. 重新加载系统服务配置：
```shell 
sudo systemctl daemon-reload 
``` 
4. 重启 Docker 服务：
```shell 
sudo systemctl restart docker 
``` 

### CentOS 配置
CentOS 的配置步骤与 Ubuntu 基本一致：
1. 创建 `/etc/docker` 目录：
```shell 
sudo mkdir -p /etc/docker 
``` 
2. 编辑 `daemon` 配置文件：
```shell 
sudo tee /etc/docker/daemon.json <<-'EOF' 
{ 
    "insecure-registries": ["ip地址:38080"] 
} 
EOF 
``` 
3. 重新加载系统服务配置：
```shell 
sudo systemctl daemon-reload 
``` 
4. 重启 Docker 服务：
```shell 
sudo systemctl restart docker 
``` 

### MacOS 配置
针对安装了 `Docker for Mac` 的用户，可按以下步骤操作：
1. 点击任务栏中的 `Docker Desktop` 应用图标，选择 `Preferences`。
2. 在左侧导航菜单中选择 `Docker Engine`。
3. 在右侧输入栏编辑 `JSON` 文件，将 `ip地址:38080` 添加到 `"registry-mirrors"` 数组中。示例如下：
```json 
{ 
    "registry-mirrors": ["ip地址:38080"] 
} 
``` 
4. 点击 `Apply & Restart` 按钮，等待 Docker 重启。

Windows 配置
对于安装了 `Docker for Windows` 的用户：
1. 在系统右下角托盘图标内右键菜单选择 `Settings`。
2. 打开配置窗口后，在左侧导航菜单选择 `Docker Daemon`。
3. 编辑窗口内的 `JSON` 串，添加以下内容：
```json 
{ 
    "insecure-registries": ["ip地址:38080"] 
} 
``` 

## 其他开源容器客户端配置

### nerdctl 配置
nerdctl 是一个与 Docker 兼容的容器命令行工具，通常与 containerd 一起使用。配置 `insecure-registries` 可通过编辑 `~/.config/containerd/config.toml` 文件：
```toml 
[plugins."io.containerd.grpc.v1.cri".registry] 
  [plugins."io.containerd.grpc.v1.cri".registry.mirrors] 
    [plugins."io.containerd.grpc.v1.cri".registry.mirrors."ip地址:38080"] 
      endpoint = ["http://ip地址:38080"] 
  [plugins."io.containerd.grpc.v1.cri".registry.configs] 
    [plugins."io.containerd.grpc.v1.cri".registry.configs."ip地址:38080".tls] 
      insecure_skip_verify = true 
``` 
配置完成后，重启 containerd 服务：
```shell 
sudo systemctl restart containerd 
``` 

### crictl 配置
crictl 是 Kubernetes CRI 的命令行接口。可以通过编辑 `/etc/crictl.yaml` 文件来配置：
```yaml 
runtime-endpoint: unix:///run/containerd/containerd.sock 
image-endpoint: unix:///run/containerd/containerd.sock 
timeout: 10 
debug: false 
pull-image-on-create: false 
disable-pull-on-run: false 
registries: 
  insecure: 
    - ip地址:38080 
``` 

### ctr 配置
ctr 是 containerd 的命令行工具。配置方法与 nerdctl 类似，编辑 `~/.config/containerd/config.toml` 文件，添加以下内容：
```toml 
[plugins."io.containerd.grpc.v1.cri".registry] 
  [plugins."io.containerd.grpc.v1.cri".registry.mirrors] 
    [plugins."io.containerd.grpc.v1.cri".registry.mirrors."ip地址:38080"] 
      endpoint = ["http://ip地址:38080"] 
  [plugins."io.containerd.grpc.v1.cri".registry.configs] 
    [plugins."io.containerd.grpc.v1.cri".registry.configs."ip地址:38080".tls] 
      insecure_skip_verify = true 
``` 
然后重启 containerd 服务。

### podman 配置
Podman 是一个无守护进程的容器引擎。可以通过编辑 `/etc/containers/registries.conf` 文件来配置：
```ini 
[registries.insecure] 
registries = ['ip地址:38080'] 
``` 

## 镜像打包命名说明
镜像命名规则为：`仓库访问 url/存储空间/仓库名称/镜像名称:版本号`。示例如下：
```shell 
docker build -t ip地址:38080/taibao/test-docker/demo:latest . 
``` 

注意事项
在本机配置时，由于 Docker 网络问题，配置 IP 地址时需填写网卡 IP，不可使用 `localhost`、`127.0.0.1` 等特殊本地地址。 
