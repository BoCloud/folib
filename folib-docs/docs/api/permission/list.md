# GET   权限列表

`GET /api/auth/queryRole`

## Body 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|page|query|string| 否 ||页码|
|limit|query|string| 否 ||每页条数|
|name|query|string| 否 ||权限名称，精确匹配|
|matchRoleName|query|string| 否 ||权限名称，模糊匹配|
|storageId|query|string| 否 ||存储空间id|
|repositoryId|query|string| 否 ||仓库id|
|path|query|string| 否 ||制品路径|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "status": 200,
  "data": {
    "total": 1,
    "rows": [
      {
        "id": "demo-auth",
        "isDefault": "0",
        "users": "demo",
        "userGroups": "demo-group"
      }
    ]
  }
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
|status|integer|true|none|状态码|200|
|data|object|true|none||none|
|total|integer|true|none|总数|none|
|rows|[object]|true|none|数据|none|
|id|string|true|none|权限id|none|
|isDefault|string|true|none|默认权限设定|是否是默认权限 1 是 0 否|
|users|string|false|none|用户列表|none|
|userGroups|string|false|none|用户组列表|none|