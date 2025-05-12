# GET 查询制品晋级状态

`GET /api/artifact/folib/promotion/info/{syncNo}`

## 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|syncNo|path|string| 是 ||晋级编号|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "sourcePath": "http://192.168.5.8:38080/demo-project/demo-local-raw/license.zip",
  "targetPath": "http://192.168.5.9:38080/test-project/test-local-raw/license.zip",
  "opsType": 1,
  "syncNo": "SyncNo64f5721c13c047a792950d7b98029e39",
  "status": 3,
  "failedReason": ""
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
|sourcePath|string|true|none|制品源路径|none|
|targetPath|string|true|none|制品目标路径|none|
|opsType|integer|true|none|类型|1 （制品晋级）2（制品分发）|
|syncNo|string|true|none|晋级编号|none|
|status|integer|true|none|状态|1（就绪）2（同步中）3（成功）4（失败）|
|failedReason|string|true|none|失败原因|none|
