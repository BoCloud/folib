# huggingface的示例



## 一、配置环境变量
在使用Hugging Face仓库之前，需要配置一些环境变量。这些变量会影响Hugging Face客户端的行为。

1. 配置ETag超时时间和端点
```bash 
export HF_HUB_ETAG_TIMEOUT=1500000000 
export HF_ENDPOINT=https://127.10.10.2/artifactory/api/huggingfaceml/hunggingface 
``` 
- `HF_HUB_ETAG_TIMEOUT`：对于0.19.0及以上版本的Hugging Face客户端，该参数允许使用管道和标记符解析模型。
- `HF_ENDPOINT`：指定Hugging Face仓库的端点地址。

2. 配置验证令牌
```bash 
export HF_TOKEN=eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6InJMU29QdTVtd0hwUkhHM0FqOVFTOXciLCJzdWIiOiJhZG1pbiIsInVzZXJIYXNoIjoiLTUwNzkxMzgyNiIsImlhdCI6MTc0NTgyNzMxOCwiZXhwIjozNjM3OTg3MzE4fQ.yOjckz0PlLlyvFp26aasppoGdlioN8JArzZz0boijg0 
``` 
该令牌用于通过FoLib验证Hugging Face客户端。

## 二、上传模型
使用Python代码上传模型到Hugging Face仓库。以下是示例代码：
```python 
from huggingface_hub import HfApi 
 
api = HfApi() 
api.upload_folder( 
    folder_path="{{folder_name}}",  # 要上传的文件夹在文件系统中的位置 
    repo_id="{{model_name}}",  # 定义模型在本地仓库中保存的名称 (models--${{model_name}}) 
    revision="{{model_revision}}",  # 表示文件存储的git版本 (默认为main) (snapshots/${{revision}}/...files) 
    repo_type="model" 
) 
``` 
代码解释
- `folder_path`：指定要上传的模型文件夹的本地路径。
- `repo_id`：模型在仓库中的唯一标识符。
- `revision`：模型的版本号，默认为`main`。
- `repo_type`：指定仓库类型为`model`。

## 三、下载模型
使用Python代码从Hugging Face仓库下载模型。以下是示例代码：
```python 
from huggingface_hub import snapshot_download 
 
snapshot_download( 
    repo_id="{{model_name}}", 
    revision="{{model_revision}}", 
    etag_timeout=1500000000 
) 
``` 
代码解释
- `repo_id`：要下载的模型在仓库中的唯一标识符。
- `revision`：要下载的模型版本号。
- `etag_timeout`：ETag超时时间，与之前配置的环境变量一致。

## 四、使用变换器和扩散器等库解析模型
如果您使用的是0.19.0及以上版本的Hugging Face客户端并启用了`HF_HUB_ETAG_TIMEOUT`参数，现在可以使用变换器和扩散器等库解析模型。具体使用方法可以参考相关库的官方文档。

## 五、仓库地址
仓库使用地址为：
``` 
https://127.10.10.2/artifactory/api/huggingfaceml/hunggingface 
``` 
您可以通过该地址访问和管理Hugging Face仓库中的模型。

:::tip
- 请确保您的Hugging Face客户端版本为0.19.0或以上，以使用`HF_HUB_ETAG_TIMEOUT`参数的相关功能。
- 在上传和下载模型时，请替换示例代码中的`{{folder_name}}`、`{{model_name}}`和`{{model_revision}}`为实际的值。
- 令牌`HF_TOKEN`具有一定的有效期，请在有效期内使用。 
:::
