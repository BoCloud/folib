# POST 设置仓库权限

`POST /api/configuration/folib/storages/{storageId}/{repositoryId}/permission`

## Body 请求参数

```json
{
  "scope": 1,
  "allowAnonymous": true
}
```

## 请求参数

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|storageId|path|string| 是 ||存储空间id|
|repositoryId|path|string| 是 ||仓库id|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|scope|body|integer| 是 | 仓库可见范围|1 存储空间内 2 公开|
|allowAnonymous|body|boolean| 是 | 匿名设定|是否允许匿名访问 true 允许 false 不允许|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```shell
"ok"
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
