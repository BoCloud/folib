# POST 制品晋级

`POST /api/artifact/folib/promotion/nodeOption`

异步接口

## Body 请求参数

```json
{
  "sourcePath": "http://192.168.5.8:38080/demo-project/demo-local-raw/license.zip",
  "targetPath": "http://192.168.5.9:38080/test-project/test-local-raw/license.zip"
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|» sourcePath|body|string| 是 | 制品源路径|制品源路径格式|
|» targetPath|body|string| 是 | 制品目标路径|制品目标路径格式|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**sourcePath**: 制品源路径格式

{http|https}\://{IP|域名}:{端口}/{存储空间id}/{仓库id}/{制品路径}

**targetPath**: 制品目标路径格式

{http|https}\://{IP|域名}:{端口}/{存储空间id}/{仓库id}/{制品路径}

> 返回示例

```shell
"SyncNo24ca3c6173f84fd183367b6277a517b2"
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
