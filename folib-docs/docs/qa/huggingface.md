# HuggingFace 使用文档

## 代理库使用

:::tip
💡 适用场景: 当本地没有大模型，通过代理的方式下载大模型
:::

1. **注册 huggingface 账号获取 token**

	1. 使用魔法访问 huggingface.co 进行登录或者注册

	![hugging face main page](../assets/folib/questions/questions-huggingface-main.png)

	2. 获取 token

	![入口](../assets/folib/questions/question-huggingface-access-token-door.png)

	![按钮](../assets/folib/questions/questions-huggingface-access-token-create-btn.png)

	![创建](../assets/folib/questions/questions-huggingface-access-token-create.png)

3. **创建代理库**

![创建 hugging face 库](../assets/folib/questions/questions-huggingface-folib-create.png)

![配置代理库](../assets/folib/questions/questions-huggingface-folib-proxy.png)

:::tip
💡 推荐使用魔法地址: https://huggingface.co/ ; 国内地址: http://hf-mirror.com/ 不稳定

🗝️ 只需要将 token 填入密码位置，不需要填写账号
:::

![远程配置](../assets/folib/questions/questions-huggingface-folib-remote-access.png)

4. **下载 AI 模型**

	+ 下载脚本

```python
from huggingface_hub import snapshot_download
import os

#超时时间
os.environ['HUGGINGFACE_HUB_TIMEOUT'] = '120'

#下载模型
try:
    #repo_id:模型仓库id(机构/模型名称 或者 模型名称) ,local_dir:本地存储目录
    snapshot_download(repo_id='google-bert/bert-base-uncased', local_dir='my-bert-base-uncased')
except Exception as e:
    print(f"An error occurred: {e}")
```

  + 下载示例

```shell
#设置环境变量
export HF_HUB_ETAG_TIMEOUT=1500000000

export HF_ENDPOINT=http://10.50.9.37:38080/storages/cl0121/hf-proxy

export HF_TOKEN=eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjItMmxGZzBRQ2FDUHJZQTlEMWxYSmciLCJzdWIiOiJhZG1pbiIsInVzZXJIYXNoIjoiLTUwNzkxMzgyNiIsImlhdCI6MTczNzUyNDczMywiZXhwIjozNjI5Njg0NzMzfQ.7tQaMPWsj4IxaKNS1HxJdXZXmQKJSy_-zDkc9ReP8dE

#执行下载脚本
python3 download.py
```

## 本地库使用

:::tip
💡 适用场景: 大模型二次开发或者本地训练后存储模型的快照
:::

1. **创建本地库**

![创建本地库](../assets/folib/questions/questions-huggingface-folib-local.png)

2. **上传脚本**

```python
from huggingface_hub import HfApi,configure_http_backend
from requests import Session

import random
import string
import os

import requests
import logging

logging.basicConfig(level=logging.DEBUG)

#源数据 folib url
api_url ="http://localhost:38080/artifactory/"

#folib token
access_token = "eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6Il9JUWxRX1gzOEE1UWNKbmJmeExTamciLCJzdWIiOiJhZG1pbiIsInVzZXJIYXNoIjoiLTE3MjE3NzE4NTQiLCJpYXQiOjE3NDIzODQxMzgsImV4cCI6MTc0MjM5ODUzOH0.Ey1U0Ex9UTaARozRRCFCOt9L9XJxu3HXv4eNi4_2fu4"

headers = { "Authorization": "Bearer " + access_token }

# 随机 SHA 生成 - 生成一个指定长度（默认为 10 个字符）的随机十六进制 SHA 字符串
def generate_random_sha(length=10):
    characters = string.hexdigits[:-6]
    return ''.join(random.choice(characters) for i in range(length))

# PUT方法的API请求 - 发送一个 PUT 请求到指定的 api_url，并带有请求头。如果请求失败，捕获异常并输出错误信息
def call_api_with_put(api_url):
  try:
    response = requests.put(api_url, headers=headers)
    response.raise_for_status()
    return response.json()
  except requests.exceptions.RequestException as e:
    print(f"aip 请求失败:{e}")

# set_properties() 函数通过 path 设置模型的属性
# def set_properties(path, properties):
#     formatted_string = '|'.join([f'{key}={value}' for key, value in properties.items()])
#     print(formatted_string)
#     url = api_url+"api/storage/{}?properties={}&recursive=0".format(path, formatted_string)
#     print(url)
#     response_data = call_api_with_put(url)

repo_model_id = "google/bert-base-uncased"
orgid="google"
print(repo_model_id)

random_sha = generate_random_sha()
print(random_sha)

# 自定义 HTTP 后端，强制为所有请求添加 Token
# 自定义 Session 类，自动添加 Token
class CustomSession(Session):
    def request(self, method, url, **kwargs):
        headers = kwargs.get("headers", {})
        if "Authorization" not in headers:
            headers["Authorization"] = f"Bearer {access_token}"
        kwargs["headers"] = headers
        return super().request(method, url, **kwargs)

# 配置自定义 HTTP 后端
configure_http_backend(backend_factory=lambda: CustomSession())
api = HfApi(token=access_token)
api.upload_folder(folder_path="tuned-1717484412.055434",repo_id=repo_model_id,repo_type="model",revision=random_sha,token=access_token)

# 设置文件属性
# set_properties(path="public-project/hfml-local/models/{}/{}".format(repo_model_id,random_sha),properties={"model_type":"bert","tunedby":"admin"})
```

3. **上传示例**

```shell
#设置环境变量
export HF_HUB_ETAG_TIMEOUT=1500000000

export HF_ENDPOINT=http://localhost:38080/artifactory/api/huggingfaceml/hfml-local

export HF_TOKEN=eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjdHZld4THhkTjdMaWFiOTR4bU5YYUEiLCJzdWIiOiJhZG1pbiIsInVzZXJIYXNoIjoiLTE3MjE3NzE4NTQiLCJpYXQiOjE3NDIzODQxODUsImV4cCI6MzYzNDU0NDE4NX0.JPo6z2I6_KrXTrAuhGJ8-0GQtSFPMR6qSYFDC_JUHU4

#执行上传脚本
python3 upload.py
```

![上传执行](../assets/folib/questions/questions-hugging-face-upload-dir.png)

![上传结果](../assets/folib/questions/questions-huggingface-folib-upload-dir-result.png)

4. **下载AI模型**

同代理库的下载脚本
