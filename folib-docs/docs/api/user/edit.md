# PUT 修改用户

`PUT /api/users/{username}`

## Body 请求参数

```json
{
  "email": "demo@folib.com",
  "enabled": true,
  "password": "J6ADocOQ9vVLU45juz023zB/IQ3JugoPmMGLYK/LdmeXtwyxkwMJpbv0pWq3eq7CriUqKDjQDwMZkS6YYvHpPr3Hfy/xxlqunzhczCC1wK2FUHcKcvz9SQJZHETA6aFZZfHJQUIW5CAV8MyE6YDChmAT7Cb90iICMmIH4bR7gwo=",
  "originalPassword": "folib@v58777",
  "roles": [
    "GENERAL"
  ],
  "username": "demo"
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|username|path|string| 是 ||用户名|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|email|body|string| 否 | 邮箱|none|
|enabled|body|boolean| 是 | 是否可用|true 可用 false 不可用|
|password|body|string| 否 | 密文密码|密文密码|
|originalPassword|body|string| 否 | 明文密码|明文密码|
|roles|body|[string]| 否 | 角色集合|none|
|userGroupIds|body|[string]| 否 | 用户组集合|none|
|username|body|string| 是 | 用户名|none|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**password**: 密文密码

`password` 与 `originalPassword` 字段必传其一
`password` 需传入 `RSA` 算法加密后的字符
`originalPassword` 传入明文密码
`password` 与 `originalPassword` 任选其一使用

**originalPassword**: 明文密码

`password` 与 `originalPassword` 字段必传其一
`password` 需传入 `RSA` 算法加密后的字符
`originalPassword` 传入明文密码
`password` 与 `originalPassword` 任选其一使用

> 返回示例

```shell
"用户更新成功."
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
