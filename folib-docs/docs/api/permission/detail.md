# GET 获取权限详情

`GET /api/auth/{name}`

## 请求参数

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|name|path|string| 是 ||权限名称|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "name": "demo-auth",
  "description": "",
  "privileges": {
    "groups": [
      {
        "id": "885934299172306945",
        "access": [
          "ARTIFACTS_DEPLOY",
          "ARTIFACTS_DELETE"
        ]
      }
    ],
    "users": [
      {
        "id": "demo",
        "access": [
          "ARTIFACTS_DEPLOY",
          "ARTIFACTS_DELETE"
        ]
      }
    ]
  },
  "resources": [
    {
      "resourceId": "DEMO-PROJECT_DEMO-LOCAL-RAW",
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw",
      "path": ""
    },
    {
      "resourceId": "DEMO-PROJECT_DEV-LOCAL-RAW",
      "storageId": "demo-project",
      "repositoryId": "dev-local-raw",
      "path": ""
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
|name|string|true|none|权限名称|none|
|description|string|false|none|权限描述|none|
|privileges|object|true|none|权限对象|none|
|groups|[object]|false|none|用户组列表|none|
|id|string|true|none|用户组id|none|
|access|[string]|true|none|权限点|none|
|users|[object]|false|none|用户列表|none|
|id|string|true|none|用户id|none|
|access|[string]|true|none|权限点|none|
|resources|[object]|true|none|资源列表|none|
|resourceId|string|true|none|资源id|none|
|storageId|string|true|none|存储空间id|none|
|repositoryId|string|false|none|仓库id|none|
|path|string|false|none|路径|none|
