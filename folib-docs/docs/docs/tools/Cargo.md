# Cargo的示例
## Cargo 简介

Cargo 是 Rust 编程语言的官方包管理器和构建工具，由 Rust 团队开发维护。它通过自动化流程简化了 Rust 项目的开发与管理，主要功能包括：

1. 项目生命周期管理
   - 创建新项目：`cargo new` 生成标准项目结构
   - 构建与运行：`cargo build` 编译项目，`cargo run` 直接执行
   - 测试与发布：`cargo test` 执行单元测试，`cargo publish` 发布到 crates.io 社区仓库

2. 智能依赖管理
   - 自动解析版本：通过 `Cargo.toml` 文件声明依赖，支持语义化版本控制（SemVer）
   - 多源支持：可配置官方仓库、镜像源或私有仓库（如企业级制品库）

3. 跨平台构建系统
   - 统一编译环境：自动处理 Rustc 编译参数和工具链版本
   - 扩展支持：通过插件机制集成 WASM、NDK 等跨平台编译需求
作为 Rust 生态的核心组件，Cargo 已成为全球超过 90% Rust 项目的标准工具链，其设计理念深刻影响了现代编程语言的包管理范式。用户可通过 `rustup` 工具链默认安装，或通过源码编译获得最新特性。
---
## 一、安装前准备
1. 系统要求：
    - Windows：需安装 )（MSVC 或 MinGW）
    - Linux/macOS：需安装 `curl`、`gcc`、`pkg-config` 等基础工具
2. 网络配置建议：
   ```bash 
   # 设置中科大镜像源（通用配置）
   export RUSTUP_DIST_SERVER=https://mirrors.ustc.edu.cn/rust-static 
   export RUSTUP_UPDATE_ROOT=https://mirrors.ustc.edu.cn/rust-static/rustup 
   # 或者配置FOLIB官方镜像源
   export RUSTUP_DIST_SERVER=https://public.folib.com/public-project/rust-static 
   export RUSTUP_UPDATE_ROOT=https://public.folib.com/public-project/rust-static/rustup 
   ```
---

## 二、各平台安装步骤

Windows 系统

1. 下载安装器
   ```powershell 
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh 
   ```
    - 或手动下载

2. 自定义安装路径  
   设置环境变量：
    - `CARGO_HOME=D:\rust\.cargo`
    - `RUSTUP_HOME=D:\rust\.rustup`

3. 选择工具链
   ```bash 
   1) Proceed with installation (default)
   2) Customize installation 
   > 输入 2 选择 x86_64-pc-windows-gnu 工具链 
   ```

Linux 系统

```bash 
自动安装（推荐）
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh 
 
手动安装（离线环境）
wget https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init 
chmod +x rustup-init 
./rustup-init --default-toolchain stable --profile default -y 
```

macOS 系统

```bash 
使用 Homebrew 安装 
brew install rustup-init 
 
或官方安装命令 
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh 
```
 
---


##  三、全局配置（所有项目生效）
1. 创建/修改Cargo配置文件
   ```bash 
   # Linux/macOS 
   vim ~/.cargo/config.toml 
 
   # Windows 
   notepad %USERPROFILE%\.cargo\config.toml 
   ```

2. 配置私有仓库源
   ```toml 
   [registries]
   folib = { index = "http://10.50.9.36:38080/artifactory/api/cargo/cargo-local" }
 
   [source]
   crates-io = { replace-with = "folib" }  # 优先使用私有仓库 
   folib = { registry = "http://10.50.9.36:38080/artifactory/api/cargo/cargo-local" }
   ```

---

## 四、项目级配置
1. Cargo.toml声明私有依赖
   ```toml 
   [dependencies]
   my-private-crate = { version = "0.1.0", registry = "folib" }
   ```

2. 多仓库混合使用场景
   ```toml 
   [source]
   crates-io = { replace-with = 'folib' }   # 默认源 
   folib = { registry = "http://10.50.9.36:38080/artifactory/api/cargo/cargo-local" }
   tuna = { registry = "https://mirrors.tuna.tsinghua.edu.cn/git/crates.io-index.git" }  # 备用镜像 
   ```

---

##  五、认证配置（需仓库管理员权限）
1. 生成访问令牌
    - 登录Folib Web界面 -> 用户中心 -> 生成API Key

2. 添加认证信息
   ```bash 
   cargo login --registry=folib 
   # 输入提示的令牌：Bearer <your-api-key>
   ```

   *或手动配置：*
   ```toml 
   [registry]
   token = "Bearer <your-api-key>"
 
   [registries.folib]
   index = "http://10.50.9.36:38080/artifactory/api/cargo/cargo-local"
   ```

---

##  六、发布私有包
1. 配置包信息
   ```toml 
   # Cargo.toml 
   [package]
   name = "my-private-crate"
   version = "0.1.0"
   publish = ["folib"]  # 指定发布目标仓库 
   ```

2. 执行发布命令
   ```bash 
   cargo publish --registry folib 
   ```

---

## 七、验证配置
1. 依赖解析测试
   ```bash 
   cargo build --verbose  # 观察依赖下载来源 
   ```

2. 仓库内容检查
   ```bash 
   curl -u admin:password http://10.50.9.36:38080/artifactory/api/cargo/cargo-local/my-private-crate 
   ```

---

## 八、高级配置（可选）
1. 代理仓库配置
   ```toml 
   [source.folib-proxy]
   registry = "http://10.50.9.36:38080/artifactory/api/cargo/cargo-proxy"
   replace-with = "folib"  # 代理仓库缓存策略 
   ```

2. 多环境配置
   ```toml 
   [target.x86_64-unknown-linux-gnu.dependencies]
   special-dep = { version = "1.0", registry = "folib-linux" }
   ```

---

> 本配置参考Folib制品库特性及Rust官方文档，如需深度定制可联系仓库管理员获取《Folib制品库技术白皮书》
