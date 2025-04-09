# PUT 新增存储空间

`PUT /api/configuration/folib/storages`

## Body 请求参数

```json
{
  "id": "demo-project",
  "basedir": "/arti-kf-bucket-poc/demo-project",
  "storageProvider": "s3",
  "storageMaxSize": 0,
  "admin": ""
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|id|body|string| 是 | 存储空间id|none|
|basedir|body|string| 否 | 基础路径|S3存储必传 NAS存储可不传|
|storageProvider|body|string| 是 | 存储空间类型|S3存储传 s3，本地存储传 local|
|storageMaxSize|body|integer| 否 | 存储配额|单位是bytes|
|admin|body|string| 否 | 存储空间管理员|none|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**basedir**: `S3` 存储必传 `NAS` 存储可不传

`S3` 格式：{桶名称}/{存储空间id} 桶名称非必填，若无桶名称，则会以存储空间id创建一个桶

> 返回示例

```shell
"The storage was created successfully."
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
