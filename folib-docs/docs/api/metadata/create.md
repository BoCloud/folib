# PUT 新增元数据（单个）

`PUT /api/artifact/artifactMetadata`

## Body 请求参数

```json
{
  "key": "NAME",
  "type": "STRING",
  "viewShow": 1,
  "value": "API.md",
  "storageId": "demo-project",
  "repositoryId": "demo-local-raw",
  "artifactPath": "API.md"
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|key|body|string| 是 | 元数据key|none|
|type|body|string| 是 | 元数据类型|元数据类型|
|viewShow|body|integer| 是 | 页面展示设定|页面是否展示 0 不展示 1 展示|
|value|body|string| 是 | 元数据值|none|
|storageId|body|string| 是 | 存储空间id|none|
|repositoryId|body|string| 是 | 仓库id|none|
|artifactPath|body|string| 是 | 制品路径|none|
|recursive|body|boolean| 否 | 是否递归|目录级别元数据 true 递归子文件夹、子文件false 不递归子文件夹、子文件|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

**type**: 元数据类型

+ NUMERICAL（数字）
+ STRING（字符串）
+ TEXT（文本）
+ MD（Markdown）
+ JSON（JSON）

> 返回示例

```shell
"ok"
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
