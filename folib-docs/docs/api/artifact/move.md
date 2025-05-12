# POST 移动制品

`POST /api/artifact/folib/promotion/move`

异步接口

## Body 请求参数

```json
{
  "path": "license.zip",
  "targetPath": "license.zip",
  "srcStorageId": "demo-project",
  "srcRepositoryId": "demo-local-raw",
  "targetRepositoyList": [
    {
      "targetStorageId": "demo-project",
      "targetRepositoryId": "dev-local-raw"
    }
  ]
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|» path|body|string| 是 | 制品源路径|none|
|» targetPath|body|string| 是 | 制品目标路径|none|
|» srcStorageId|body|string| 是 | 制品源存储空间id|none|
|» srcRepositoryId|body|string| 是 | 制品源仓库id|none|
|» targetRepositoyList|body|[object]| 是 | 目标仓库列表|none|
|»» targetStorageId|body|string| 是 | 制品目标存储空间id|none|
|»» targetRepositoryId|body|string| 是 | 制品目标仓库id|none|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```shell
"Artifact moving"
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
