# GET 浏览仓库内容

`GET /api/browse/{storageId}/{repositoryId}/{path}`

## Body 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|storageId|path|string| 是 ||存储空间id|
|repositoryId|path|string| 是 ||仓库id|
|path|path|string| 是 ||制品路径|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "directories": [
    {
      "name": "09-10",
      "size": null,
      "lastModified": null,
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw",
      "artifactPath": "09-10",
      "url": "http://192.168.5.8:38080/api/browse/demo-project/demo-local-raw/09-10",
      "path": "s3://admin@192.168.5.8/arti-kf-bucket-poc/demo-project/demo-local-raw/09-10"
    }
  ],
  "files": [
    {
      "name": "API.md",
      "size": 17283,
      "lastModified": 1725960488794,
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw",
      "artifactPath": "API.md",
      "url": "http://192.168.5.8:38080/storages/demo-project/demo-local-raw/API.md",
      "path": "s3://admin@192.168.5.8/arti-kf-bucket-poc/demo-project/demo-local-raw/API.md"
    },
    {
      "name": "license.zip",
      "size": 4368,
      "lastModified": 1725960512502,
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw",
      "artifactPath": "license.zip",
      "url": "http://192.168.5.8:38080/storages/demo-project/demo-local-raw/license.zip",
      "path": "s3://admin@192.168.5.8/arti-kf-bucket-poc/demo-project/demo-local-raw/license.zip"
    }
  ]
}
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|

> 返回数据结构

状态码 **200**

|名称|类型|必选|约束|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|directories|[object]|true|none|目录列表|none|
|name|string|true|none|目录名称|none|
|storageId|string|true|none|存储空间id|none|
|repositoryId|string|true|none|仓库id|none|
|artifactPath|string|true|none|制品路径|none|
|url|string|true|none|浏览地址|none|
|path|string|true|none|存储路径|none|
|files|[object]|true|none|文件列表|none|
|name|string|true|none|文件名称|none|
|size|integer|true|none|文件大小|none|
|lastModified|integer|true|none|最后修改时间|none|
|storageId|string|true|none|存储空间id|none|
|repositoryId|string|true|none|仓库id|none|
|artifactPath|string|true|none|制品路径|none|
|url|string|true|none|浏览地址|none|
|path|string|true|none|存储路径|none|
