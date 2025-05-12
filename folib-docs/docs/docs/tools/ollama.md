# Ollama仓库工具使用示例

## 一、Ollama环境安装
### Windows 系统
1. **安装步骤**
    - 下载安装包：[OllamaSetup.exe](https://ollama.com/download)
    - 双击安装（默认路径为 `C:\Users\username\AppData\Local\Programs\Ollama`）
    - 验证安装：`ollama --version` 输出版本号即成功 

2. **环境配置**
    - **修改模型存储路径**：添加系统变量 `OLLAMA_MODELS=新路径`（如 `E:\ollama\models`）
    - **开放局域网访问**：设置 `OLLAMA_HOST=0.0.0.0` 和 `OLLAMA_ORIGINS=*`
    - **端口修改**：通过 `OLLAMA_PORT=8080` 指定端口

---

### Linux 系统
1. **安装步骤**
    - 一键安装：`curl -fsSL https://ollama.com/install.sh  | sh` 
    - 手动安装（国内推荐）：通过 ModelScope 下载离线包后运行安装脚本 

2. **环境配置**
    - **修改模型路径**：
      ```bash 
      mkdir /path/to/new_models 
      export OLLAMA_MODELS="/path/to/new_models"  # 写入 ~/.bashrc 或 ~/.zshrc 
      ```  
    - **服务管理**：
      ```bash 
      sudo systemctl enable ollama  # 设为开机自启 [6]()
      ```

---

### macOS 系统
1. **安装步骤**
    - 通过 Homebrew：`brew install ollama`
    - 或直接下载 `.dmg` 安装包运行 

2. **环境配置**
    - **自定义模型路径**：
      ```bash 
      mkdir ~/ollama_models 
      export OLLAMA_MODELS="$HOME/ollama_models"  # 写入 ~/.zshrc [3]()[9]()
      ```  
    - **调试日志**：设置 `OLLAMA_DEBUG=1` 查看详细日志。

---

### 通用环境变量
| 变量名                 | 作用                              | 示例值                |
|-----------------------|-----------------------------------|----------------------|
| `OLLAMA_MODELS`       | 模型存储目录（避免C盘占满）       | `E:\ollama\models`   |
| `OLLAMA_HOST`         | 监听地址（0.0.0.0允许外网访问）  | `0.0.0.0`            |
| `OLLAMA_KEEP_ALIVE`   | 模型内存驻留时间（默认5分钟）     | `300`（秒）          |
| `OLLAMA_MAX_QUEUE`    | 最大请求队列数（默认512）         | `1024`               |

> 配置后需重启终端或服务生效  
> 完整配置参考：[Ollama官方文档](https://ollama.com) 

## 二、Ollama模型上传
:::tip
注意：Ollama模型模型仓库目前只支持，在本地上传模型，目前版本还不支持代理下载。
:::
1. **下载模型，并拷贝模型更换模型仓库地址**
```bash
ollama pull gemma3:1b 
#下载完成后
ollama cp gemma3:1b demo2.folib.com/ollama/gemma3:1b
```
2**上传**
```bash
# 上传模型
ollama push demo2.folib.com/ollama/gemma3:1b
```
3. **验证**
```bash
# 验证模型
ollama pull demo2.folib.com/ollama/gemma3:1b
```
## 三、Ollama模型使用
1. **启动服务**
```bash
# 启动服务
ollama demo2.folib.com/ollama/gemma3:1b
```
