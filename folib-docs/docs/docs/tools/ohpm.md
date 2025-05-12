# Ohpm的示例 

以下是基于官方文档和实践整理的 ohpm 使用文档，涵盖安装配置、核心功能、发布流程及常见问题解决方案，并结合folib仓库的实际使用案例。
 
---

## 一、环境安装与配置
1. 安装 DevEco Studio
- 从获取 DevEco Studio，安装时会自动集成 ohpm。
- 验证安装：命令行输入 `ohpm -v`，显示版本号即成功（如 `1.6.0`）。

2. 配置环境变量
- Windows：
    - 添加系统变量 `OHPM_HOME`，值为 ohpm 安装路径（如 `C:\Users\user\ohpm`）。
    - 将 `%OHPM_HOME%\bin` 加入 `Path` 变量。
- Mac/Linux：
    - 编辑 `.bash_profile` 或 `.zshrc`，添加 `export PATH=$PATH:/path/to/ohpm/bin`。
    - 执行 `source ~/.zshrc` 生效。

---

## 二、核心功能与命令
1. 依赖管理
- 安装库：`ohpm install @ohos/库名` （自动写入 `oh-package.json5`）。
- 全局安装：`ohpm install -g 库名`。
- 更新依赖：`ohpm update`。

2. 项目初始化
- 创建新项目时，通过 DevEco Studio 选择模板，自动生成 `oh-package.json5` 文件。

3. 脚本执行
- 自定义脚本别名：在 `oh-package.json5` 的 `scripts` 字段定义。
- 运行脚本：`ohpm run 脚本别名`（支持参数覆盖和链式调用）。

---

## 三、三方库发布流程
1. 密钥配置
- 生成密钥：
  ```bash 
  ssh-keygen -m PEM -t RSA -b 4096 -f E:\my_key_path 
  ```
  注意：必须设置 passphrase（密码短语），否则发布失败。
- 配置私钥路径：
  ```bash 
  ohpm config set key_path E:\my_key_path 
  ```

2. 仓库与用户设置
- 设置私有仓库：
  ```bash 
  ohpm config set registry http://10.50.9.37:38080/your_repo 
  ohpm config set publish_registry http://10.50.9.37:38080/your_repo 
  ```
- 用户认证（ohpm <1.6.0）：
  ```bash 
  ohpm config set publish_id Base64(用户名:密码)  # 例如 ZXhhbXBsZTpwYXNzd29yZA== 
  ```

3. 构建与发布
- 构建 HAR 包：
  在项目根目录执行构建命令（参考 DevEco Studio 构建文档），生成 `.har` 文件。
- 发布包：
  ```bash 
  cd build/outputs/default 
  ohpm publish library.har 
  ```

---

## 四、代理与安全配置
1. 代理设置
- 全局代理：
  ```bash 
  ohpm config set registry http://10.50.9.37:38080/ohpm_proxy
  ohpm config set strict_ssl false  # 关闭 SSL 校验（内网无https环境常用）
  ```

---

# 五、故障排查
1. 常见错误
- 私钥无密码：  
  现象：`Private key without passphrase is not supported`  
  解决：重新生成带 passphrase 的密钥对。

- 找不到 .har 文件：  
  现象：`No .har/.tgz file path provided`  
  解决：确认构建流程正确，且路径位于 `build/outputs/default`。

- 仓库连接超时：  
  现象：`ETIMEDOUT`  
  解决：检查代理配置或网络防火墙，使用 `ohpm config list -j` 查看当前仓库设置。

---

# 六、扩展资源
- 官方中心仓：（搜索三方库）。
- CLI 命令大全：执行 `ohpm --help` 查看完整命令列表。

---

> 提示：本文档整合了官方指南、社区实践及开发者经验，如需更深入的技术细节，可参考中的实战案例。
