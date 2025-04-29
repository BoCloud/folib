# GitLFS仓库工具使用示例

## 一、Git LFS 环境安装
1. 操作系统适配安装
   Windows 用户
- 从  下载 `.exe` 安装程序，双击运行并按照向导完成安装。
- 安装完成后需添加环境变量：将 `C:\Program Files\Git\mingw64\bin` 添加到系统变量 Path 中。

macOS 用户
- 通过 Homebrew 安装：
  ```bash 
  brew install git-lfs 
  ```  
  或从官网下载 `.pkg` 安装包。

Linux 用户
- Debian/Ubuntu：
  ```bash 
  curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash 
  sudo apt-get install git-lfs 
  ```  
- CentOS/RHEL：
  ```bash 
  curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.rpm.sh | sudo bash 
  sudo yum install git-lfs 
  ```  


2. 验证安装
   安装完成后，执行以下命令验证：
```bash 
git lfs version  # 显示版本号（如 git-lfs/3.3.0）即表示成功
git lfs install  # 首次初始化，输出 "Git LFS initialized" 表示配置完成
```
 
---

## 二、私有化仓库配置强化说明
1. 全局 LFS 地址配置
```bash 
git config --global lfs.url https://127.10.10.2/artifactory/api/lfs/gitlfs 
```  
（适用于所有本地仓库，避免每个项目重复配置）

2. 单仓库 LFS 地址配置
   若需覆盖全局配置，可在仓库目录下执行：
```bash 
git config --local lfs.url [私有仓库地址]
```

3. 强制 HTTPS 协议支持
   若私有仓库使用自签名证书，需添加证书信任：
```bash 
git config --global http.sslVerify false  # 临时禁用 SSL 验证（慎用）
```  
或配置系统信任证书。
 
---

## 三、常见问题排查
1. LFS 文件未正常下载
- 现象：克隆后大文件显示为指针文件（如 `oid sha256:...`）。
- 解决：手动触发下载：
  ```bash 
  git lfs pull 
  ```  
  或克隆时添加参数：
  ```bash 
  git clone --config lfs.url=[私有地址] [仓库URL]
  ```  


2. 权限认证失败
- 现象：`LFS authentication required`。
- 解决：
   1. 生成 SSH 密钥并添加到私有仓库服务器。
   2. 若使用 HTTPS，配置凭证缓存：
      ```bash 
      git config --global credential.helper store 
      ```  

 
---

## 四、扩展建议
- 大文件类型管理：通过 `.gitattributes` 文件批量定义追踪规则（如 `*.zip filter=lfs diff=lfs merge=lfs -text`）。
- 存储空间优化：定期清理本地 LFS 缓存：
  ```bash 
  git lfs prune 
  ```  

> 更多进阶操作可参考： 
