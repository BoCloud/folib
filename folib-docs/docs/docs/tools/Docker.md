# Docker的示例

该示例说明各个不同类型的操作系统如何配置 `Docker` 仓库，当然您已可以直接在仓库的使用说明按钮中进行查看。

## Ubuntu配置

针对 `Docker` 客户端版本大于 `1.10.0` 的用户

您可以通过修改`daemon`配置文件`/etc/docker/daemon.json`来使用:

```shell
sudo mkdir -p /etc/docker
sudo tee /etc/docker/daemon.json <<-'EOF'
{
"insecure-registries": ["ip地址:38080"]
}
EOF
sudo systemctl daemon-reload
sudo systemctl restart docker
```

## CentOS配置

针对 `Docker` 客户端版本大于 `1.10.0` 的用户

您可以通过修改 `daemon` 配置文件 `/etc/docker/daemon.json` 来使用:

```shell
sudo mkdir -p /etc/docker
sudo tee /etc/docker/daemon.json <<-'EOF'
{
"insecure-registries": ["ip地址:38080"]
}
EOF
sudo systemctl daemon-reload
sudo systemctl restart docker
```

## MacOS配置

针对安装了 `Docker for Mac` 的用户，您可以参考以下配置步骤：

在任务栏点击 `Docker Desktop` 应用图标 `-> Perferences`，在左侧导航菜单选择 `Docker Engine`，在右侧输入栏编辑 `json` 文件。将:

`ip地址:38080`加到"registry-mirrors"的数组里，点击 `Apply & Restart`按钮，等待Docker重启

## Windows配置

针对安装了 `Docker for Windows` 的用户，您可以参考以下配置步骤：

在系统右下角托盘图标内右键菜单选择 `Settings`，打开配置窗口后左侧导航菜单选择  `Docker Daemon` 。编辑窗口内的 `JSON` 串，填写下方地址：

```shell
{
"insecure-registries": ["ip地址:38080"]
}
```

## 镜像打包命名说明

请一定要看，这决定了你的镜像包能否上传

镜像命名规则如下： `仓库访问url/存储空间/仓库名称/镜像名称:版本号` ，具体如下：

```shell
docker build -t ip地址:38080/taibao/test-docker/demo:latest .
```

:::warning 提醒
请注意，本机配置时，因Docker网络问题，在配置ip地址时，要填写网卡IP，不可使用localhost ，127.0.0.1等特殊本地地址
:::