# PUT 新增用户组

`PUT /api/groups`

## Body 请求参数

```json
{
  "groupName": "demo-group",
  "description": "",
  "userIds": [
    "demo"
  ],
  "joinGroup": "1"
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 |认证信息||
|body|body|object| 否 ||none|
|groupName|body|string| 是 | 用户组名称|none|
|description|body|string| 否 | 用户组描述|none|
|userIds|body|[string]| 否 | 用户id列表|none|
|joinGroup|body|string| 是 | 加入用户组设定|是否自动将新用户加入此用户组|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**joinGroup**: 是否自动将新用户加入此用户组

1 是 0 否

> 返回示例

```shell
"用户组创建成功."
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
