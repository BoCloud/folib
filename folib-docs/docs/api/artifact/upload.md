# 上传文件（支持批量）

`POST /api/artifact/folib/promotion/upload-files`

## Body 请求参数

```yaml
storageId: demo-project
repostoryId: demo-local-raw
filePathMap: '{"API.md":"09-10/API.md"}'
files:
  - （二进制）
fileMetaDataMap: '{"09-10/API.md":{"ID":{"value
  ":"1","type":"STRING","viewShow":1},"NAME":{"value
  ":"agoh-crypto-1.4.6-304.ha","type":"STRING","viewShow":1}}}'
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||multipart/form-data|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|storageId|body|string| 是 ||存储空间id|
|repostoryId|body|string| 是 ||仓库id|
|filePathMap|body|string| 是 ||文件路径|
|files|body|[string]| 是 ||文件列表|
|fileMetaDataMap|body|string| 否 ||元数据|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**filePathMap**: 文件路径

{"文件名":"制品库存储路径"}

**fileMetaDataMap**: 元数据

`{"制品库存储路径":"元数据信息"}`

元数据信息格式：

```json
{
  "元数据key1": {
    "value":"元数据value1","type":"STRING",
    "viewShow":1
  },
  "元数据key2": {
    "value":"元数据value2","type":"STRING","viewShow":1
  }
}
```

> 返回示例

```shell
"ok"
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
