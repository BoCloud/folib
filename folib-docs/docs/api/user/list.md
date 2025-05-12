# POST 获取用户列表

`POST /api/users/queryUser`

## Body 请求参数

```json
{
  "username": "demo",
  "matchUsername": "demo",
  "email": "",
  "roles": []
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|page|query|integer| 否 ||页码|
|limit|query|integer| 否 ||每页条数|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|username|body|string| 否 | 用户名|用户名，精确匹配|
|matchUsername|body|string| 否 | 用户名|用户名，模糊匹配|
|email|body|string| 否 | 邮箱|none|
|roles|body|[string]| 否 | 角色|none|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "status": 200,
  "data": {
    "total": 11,
    "rows": [
      {
        "username": "demo",
        "enabled": true,
        "email": "demo@folib.com",
        "roles": [
          "demo-auth",
          "GENERAL"
        ],
        "userGroups": [
          "GENERAL_GROUP",
          "demo-group"
        ],
        "userGroupIds": [
          "1",
          "885934299172306945"
        ]
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
|data|object|true|none|数据|none|
|total|integer|true|none|总数|none|
|rows|[object]|true|none|行数|none|
|username|string|false|none|用户名|none|
|enabled|boolean|false|none|是否可用|true 可用 false 不可用|
|email|string|false|none|邮箱|none|
|roles|[string]|false|none|角色集合|none|
|userGroups|[string]|false|none|用户组名称集合|none|
|userGroupIds|[string]|false|none|用户组id集合|none|
