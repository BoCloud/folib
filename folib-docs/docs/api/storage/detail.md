# GET 获取存储空间信息

`GET /api/configuration/folib/storages/{storageId}`

## Body 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|storageId|path|string| 是 ||存储空间id|
|filter|query|boolean| 否 ||是否按照权限过滤仓库信息 true 过滤 其他 不过滤|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  id": "demo-project",
  "basedir": "/arti-kf-bucket-poc/demo-project",
  "admin": "admin",
  "storageProvider": "s3",
  "storageMaxSize": 1099511627776,
  "users": [
    "admin"
  ],
  "repositories": [
    {
      "id": "demo-local-raw",
      "basedir": "/arti-kf-bucket-poc/demo-project/demo-local-raw",
      "policy": "release",
      "storageProvider": "s3",
      "layout": "Raw",
      "subLayout": "raw",
      "type": "hosted",
      "secured": false,
      "status": "In Service",
      "artifactMaxSize": 104857600,
      "trashEnabled": true,
      "allowsForceDeletion": true,
      "allowsDeployment": true,
      "allowsRedeployment": true,
      "allowsDeletion": true,
      "allowsDirectoryBrowsing": true,
      "vulnerabilityWhites": [],
      "vulnerabilityBlacks": [],
      "scope": 1,
      "allowAnonymous": true,
      "storageId": "demo-project"
    },
    {
      "id": "dev-local-raw",
      "basedir": "/arti-kf-bucket-poc/demo-project/dev-local-raw",
      "policy": "release",
      "storageProvider": "s3",
      "layout": "Raw",
      "subLayout": "raw",
      "type": "hosted",
      "secured": false,
      "status": "In Service",
      "artifactMaxSize": 104857600,
      "trashEnabled": true,
      "allowsForceDeletion": true,
      "allowsDeployment": true,
      "allowsRedeployment": true,
      "allowsDeletion": true,
      "allowsDirectoryBrowsing": true,
      "vulnerabilityWhites": [],
      "vulnerabilityBlacks": [],
      "scope": 1,
      "allowAnonymous": true,
      "storageId": "demo-project"
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
|id|string|true|none|存储空间id|none|
|basedir|string|true|none|基础路径|none|
|admin|string|true|none|存储空间管理员|none|
|storageProvider|string|true|none|存储空间类型|s3（S3存储）local（本地存储 ）|
|storageMaxSize|integer|true|none|存储配额|none|
|users|[string]|true|none|存储空间下用户列表|none|
|repositories|[object]|true|none|存储空间下仓库列表|none|
|allowsDeletion|boolean|true|none|删除设定|是否允许删除 true 允许 false 不允许|
|allowsDeployment|boolean|true|none|上传设定|是否允许上传制品 true 允许 false 不允许|
|allowsDirectoryBrowsing|boolean|true|none|目录浏览设定|是否允许浏览制品 true 允许 false 不允许|
|allowsForceDeletion|boolean|true|none|强制删除设定|是否允许强制删除（会删除制品） true 允许 false 不允许|
|allowsRedeployment|boolean|true|none|覆盖设定|是否允许覆盖制品 true 允许 false 不允许|
|artifactMaxSize|integer|true|none|制品最大值|单位是bytes|
|basedir|string|true|none|基础路径|S3存储必传 NAS存储可不传 S3格式：{存储空间basedir}/{仓库id}|
|id|string|true|none|仓库id|none|
|layout|string|true|none|仓库包类型|Maven 2、NuGet、npm、PyPi、Raw、rpm、helm、conan、Docker、pub、php、cocoapods、go、GitLfs、HuggingFace|
|subLayout|string|true|none|仓库包子类型|maven、ivy、sbt、gradle、nuget、npm、yarn、ohpm、pypi、raw、rpm、helm、conan、docker、pub、php、cocoapods、go、gitlfs、huggingface|
|policy|string|true|none|版本策略|版本策略<br />release（正式版本）<br />snapshot （快照版本）<br />mixed （混合版本）|
|status|string|true|none|仓库状态|仓库状态<br />In Service（开放） Out of Service（关闭）|
|storageProvider|string|true|none|存储类型|S3存储传 s3，本地存储传 local|
|trashEnabled|boolean|true|none|回收站|是否启用回收站 true 启用 false 不启用|
|type|string|true|none|仓库类型|仓库类型<br />hosted（本地库）<br />proxy（代理库）<br />group（组合库）|
|storageId|string|true|none|存储空间id|none|
|scope|string|true|none|仓库可见范围|1 存储空间内 2 公开|
|allowAnonymous|boolean|true|none|匿名设定|是否允许匿名访问 true 允许 false 不允许|
|vulnerabilityWhites|[string]|false|none|漏洞白名单列表|none|
|vulnerabilityBlacks|[string]|false|none|漏洞黑名单列表|none|