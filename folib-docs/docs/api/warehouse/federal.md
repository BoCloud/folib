# PUT 设置联邦仓库

`PUT /api/configuration/folib/storages/{storageId}/{repositoryId}/unionRepository`

## Body 请求参数

```json
{
  "enable": true,
  "syncType": 1,
  "artifactoryType": 1,
  "artifactPaths": [
    "*"
  ],
  "metadataKey": "",
  "metadataValue": "",
  "unionTargetRepositories": [
    {
      "node": "TEST",
      "type": "inner",
      "storageId": "demo-project",
      "repositoryId": "dev-local-raw"
    }
  ]
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|storageId|path|string| 是 ||存储空间id|
|repositoryId|path|string| 是 ||仓库id|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|enable|body|boolean| 是 | 联邦仓库制品晋级设定|true 开启 false 关闭|
|syncType|body|integer| 是 | 晋级规则|1 包路径 2 元数据|
|artifactPaths|body|[string]| 否 | 包路径列表|none|
|metadataKey|body|string| 否 | 元数据key|none|
|metadataValue|body|string| 否 | 元数据value|none|
|unionTargetRepositories|body|[object]| 否 | 目标仓库列表|none|
|node|body|string| 是 | 节点标识|none|
|type|body|string| 是 | 节点类型|inner（Folib内部节点）JFrog（JFrog节点）|
|storageId|body|string| 是 | 存储空间id|none|
|repositoryId|body|string| 是 | 仓库id|none|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```shell
"The repository was updated successfully."
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
