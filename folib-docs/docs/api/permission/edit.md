# PUT 修改权限

`PUT /api/auth/{id}`

## Body 请求参数

```json
{
  "name": "demo-auth",
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
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw"
    },
    {
      "storageId": "demo-project",
      "repositoryId": "dev-local-raw"
    }
  ]
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|---|---|---|---|---|---|
|id|path|string| 是 ||权限id|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|name|body|string| 是 | 权限名称|none|
|privileges|body|object| 是 | 权限对象|none|
|groups|body|[object]| 否 | 用户组列表|none|
|id|body|string| 是 | 用户组id|none|
|access|body|[string]| 是 | 权限点|权限点|
|users|body|[object]| 否 | 用户列表|none|
|id|body|string| 是 | 用户id|none|
|access|body|[string]| 是 | 权限点|none|
|resources|body|[object]| 是 | 资源列表|none|
|storageId|body|string| 是 | 存储空间id|none|
|repositoryId|body|string| 否 | 仓库id|none|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**access**: 权限点

ARTIFACTS_DEPLOY（部署/缓存 ）
ARTIFACTS_DELETE（删除/更新）

> 返回示例

```shell
"角色更新成功."
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
