# GET 获取用户信息

`GET /api/users/{username}`

## Body 请求参数

> 请求参数

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|username|path|string| 是 ||用户名|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "user": {
    "username": "demo",
    "enabled": true,
    "email": "demo@folib.com",
    "roles": [
      "demo-auth",
      "GENERAL"
    ],
    "authorities": [
      "ADMIN",
      "ADMIN_CREATE_REPO",
      "ADMIN_DELETE_REPO",
      "ADMIN_LIST_REPO",
      "ADMIN_UPDATE_REPO",
      "ARTIFACTS_COPY",
      "ARTIFACTS_DELETE",
      "ARTIFACTS_DEPLOY",
      "ARTIFACTS_MOVE",
      "ARTIFACTS_PROMOTION",
      "ARTIFACTS_RESOLVE",
      "ARTIFACTS_VIEW",
      "AUTHENTICATED_USER",
      "COMPONENTS_VIEW",
      "CONFIGURATION_ADD_LOGGER",
      "CONFIGURATION_ADD_UPDATE_METADATA",
      "CONFIGURATION_ADD_UPDATE_REPOSITORY",
      "CONFIGURATION_ADD_UPDATE_SECURITY_POLICY",
      "CONFIGURATION_ADD_UPDATE_STORAGE",
      "CONFIGURATION_DELETE_LOGGER",
      "CONFIGURATION_DELETE_METADATA_CONFIGURATION",
      "CONFIGURATION_DELETE_REPOSITORY",
      "CONFIGURATION_DELETE_SECURITY_POLICY_CONFIGURATION",
      "CONFIGURATION_DELETE_STORAGE_CONFIGURATION",
      "CONFIGURATION_RETRIEVE_LOG",
      "CONFIGURATION_RETRIEVE_LOGBACK_CFG",
      "CONFIGURATION_SET_BASE_URL",
      "CONFIGURATION_SET_GLOBAL_PROXY_CFG",
      "CONFIGURATION_SET_INSTANCE_NAME",
      "CONFIGURATION_SET_PORT",
      "CONFIGURATION_UPDATE_LOGGER",
      "CONFIGURATION_UPLOAD",
      "CONFIGURATION_UPLOAD_LOGBACK_CFG",
      "CONFIGURATION_VIEW",
      "CONFIGURATION_VIEW_BASE_URL",
      "CONFIGURATION_VIEW_GLOBAL_PROXY_CFG",
      "CONFIGURATION_VIEW_INSTANCE_NAME",
      "CONFIGURATION_VIEW_METADATA_CONFIGURATION",
      "CONFIGURATION_VIEW_PORT",
      "CONFIGURATION_VIEW_REPOSITORY",
      "CONFIGURATION_VIEW_SECURITY_POLICY_CONFIGURATION",
      "CONFIGURATION_VIEW_STORAGE_CONFIGURATION",
      "CONFIGURE_LOGS",
      "CREATE_ROLE",
      "CREATE_USER",
      "CREATE_USER_GROUP",
      "DELETE_ROLE",
      "DELETE_USER",
      "DELETE_USER_GROUP",
      "EXTERNAL_NODE_DELETE",
      "EXTERNAL_NODE_SAVE",
      "EXTERNAL_NODE_UPDATE",
      "EXTERNAL_NODE_VIEW",
      "GLOBAL_CONFIGURATION_MANAGE",
      "IMPERSONATE_USER",
      "LICENSES_VIEW",
      "MANAGEMENT_DELETE_ALL_TRASHES",
      "MANAGEMENT_DELETE_METADATA",
      "MANAGEMENT_DELETE_TRASH",
      "MANAGEMENT_REBUILD_INDEXES",
      "MANAGEMENT_REBUILD_METADATA",
      "MANAGEMENT_UNDELETE_ALL_TRASHES",
      "MANAGEMENT_UNDELETE_TRASH",
      "RSS_FEED",
      "SEARCH_ARTIFACTS",
      "UI_BROWSE",
      "UI_LOGIN",
      "UPDATE_ROLE",
      "UPDATE_USER",
      "UPDATE_USER_GROUP",
      "VIEW_ANY_TOKEN",
      "VIEW_LOGS",
      "VIEW_OWN_TOKEN",
      "VIEW_ROLE",
      "VIEW_USER",
      "VIEW_USER_GROUP",
      "VULNERABILITIES_DATABASE_VIEW"
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
|user|object|true|none|用户信息|none|
|username|string|true|none|用户名|none|
|enabled|boolean|true|none|是否可用|true 可用 false 不可用|
|email|string|false|none|邮箱|none|
|roles|[string]|false|none|角色列表|none|
|authorities|[string]|true|none|权限点列表|none|
|userGroups|[string]|false|none|用户组列表|none|
|userGroupIds|[string]|false|none|用户组id列表|none|
