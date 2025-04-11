# JFRog API适配列表

:::tip
以下API接口的URL/请求参数/返回参数与 JFrog Aartifactory 产品保持一致和适配，如果您有使用到这些接口，那么,恭喜你🎉🎉，不需要做任何改造，直接使用即可。

:::
## 接口调用授权
|类型|请求头key|请求头value|说明|
|---|---|---|---|
|Basic认证|Authorization|Basic Base64Encode(username:password)|username为用户的用户名，password为用户的密码，Base64编码|
|Bearer认证|Authorization|Bearer access_token|access_token为系统颁发的JSON WEB TOKEN，通过系统上的访问令牌功能可以生成|


## 1、查询元数据
#### 请求 URL
```
GET http://192.168.5.100/artifactory/api/storage/{repositoryId}/{artifactPath}
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| properties | query | string | 否 | 要查询的元数据 key，多个 key 用逗号分隔 |

#### 示例请求
```
GET http://192.168.5.100/artifactory/api/storage/local-raw/demo.tar.gz?properties=name,id
```

#### 响应参数
具体响应格式取决于 JFrog Artifactory 的实现，通常会包含请求的元数据信息。

## 2、修改元数据
#### 请求 URL
```
PUT http://192.168.5.100/artifactory/api/storage/{repositoryId}/{artifactPath}
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| properties | query | string | 是 | 要修改的元数据，格式为 key=value;key=value |

#### 示例请求
```
PUT http://192.168.5.100/artifactory/api/storage/local-raw/demo.tar.gz?properties=id=1;name=test
```

#### 响应参数
成功时返回操作结果信息，失败时返回错误信息。

## 3、查询制品(AQL)
#### 请求 URL
```
POST http://192.168.5.100/artifactory/api/search/aql
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| body | body | string | 是 | AQL 查询语句，用于指定查询条件 |

#### 示例请求
```
POST http://192.168.5.100/artifactory/api/search/aql
{
    "items.find": {
        "repo": "local-raw",
        "path": {
            "$match": "."
        },
        "name": {
            "$match": "*.tar.gz",
            "$match": "*demo*"
        },
        "@name": {
            "$eq": "test"
        },
        "type": "file"
    },
    "include": [
        "name",
        "type",
        "size",
        "updated",
        "modified",
        "@name",
        "@value",
        "@id"
    ]
}
```

#### 响应参数
返回符合查询条件的制品信息列表。

## 4、查询镜像
#### 请求 URL
```
POST http://192.168.5.100/ui/api/v1/ui/views/dockerv2
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| body | body | object | 是 | 包含仓库键和镜像路径的 JSON 对象 |

#### 示例请求
```
POST http://192.168.5.100/ui/api/v1/ui/views/dockerv2
{
    "view": "dockerv2",
    "repoKey": "local-docker",
    "path": "demo/v1"
}
```

#### 响应参数
返回查询到的镜像信息。

## 5、制品晋级
#### 请求 URL
```
POST http://192.168.5.100/artifactory/api/copy/{repositoryId}/{artifactPath}
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| to | query | string | 是 | 目标仓库和制品路径，格式为 {repositoryId}/{artifactPath} |

#### 示例请求
```
POST http://192.168.5.100/artifactory/api/copy/local-raw/demo.tar.gz?to=local-raw1/demo.tar.gz
```

#### 响应参数
返回操作结果信息。

## 6、镜像晋级
#### 请求 URL
```
POST http://192.168.5.100/artifactory/api/docker/{repositoryId}/v2/promote
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| body | body | object | 是 | 包含目标仓库、镜像名称和是否复制的 JSON 对象 |

#### 示例请求
```
POST http://192.168.5.100/artifactory/api/docker/local-docker/v2/promote
{
    "targetRepo": "local-docker1",
    "dockerRepository": "demo",
    "copy": true
}
```

#### 响应参数
返回操作结果信息。

## 7、制品上传
#### 请求 URL
```
PUT http://192.168.5.100/artifactory/{repositoryId}/{artifactPath}
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| file | body | file | 是 | 要上传的文件 |

#### 示例请求
```
PUT http://192.168.5.100/artifactory/local-raw/ceshi.txt
（上传文件 ceshi.txt）
```

#### 响应参数
返回上传结果信息。

## 8、制品下载
#### 请求 URL
```
GET http://192.168.5.100/artifactory/{repositoryId}/{artifactPath}
```

#### 请求参数
无

#### 示例请求
```
GET http://192.168.5.100/artifactory/local-raw/ceshi.txt
```

#### 响应参数
返回要下载的文件内容。

## 9、创建用户组
#### 请求 URL
```
PUT http://192.168.5.100/artifactory/api/security/groups/${groupName}
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| body | body | object | 是 | 包含用户组信息的 JSON 对象 |

#### 示例请求
```
PUT http://192.168.5.100/artifactory/api/security/groups/test-group
{
    "adminPrivileges": "",
    "autoJoin": "",
    "description": "",
    "groupName": "test-group",
    "manageResources": "",
    "policyManager": "",
    "reportsManager": "",
    "usersInGroup": [
        "user1"
    ],
    "watchManager": ""
}
```

#### 响应参数
返回创建结果信息。

## 10、创建或替换权限目标
#### 请求 URL
```
PUT http://192.168.5.100/artifactory/api/security/permissions/:permissionTargetName
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| authenticated | query | string | 否 | 认证信息 |
| authorities | query | string | 否 | 权限信息 |
| credentials | query | string | 否 | 凭证信息 |
| details | query | string | 否 | 详细信息 |
| principal | query | string | 否 | 主体信息 |
| body | body | object | 是 | 包含权限目标信息的 JSON 对象 |

#### 示例请求
```
PUT http://192.168.5.100/artifactory/api/security/permissions/test-permission?authenticated=&authorities=&credentials=&details=&principal=
{
    "excludesPattern": "",
    "includesPattern": "",
    "name": "test-permission",
    "principals": {
        "groups": {},
        "users": {}
    },
    "repositories": [
        "local-raw"
    ]
}
```

#### 响应参数
返回操作结果信息。

## 11、获取用户的详细信息
#### 请求 URL
```
GET http://192.168.5.100/artifactory/api/security/users/${userName}
```

#### 请求参数
无

#### 示例请求
```
GET http://192.168.5.100/artifactory/api/security/users/test-user
```

#### 响应参数
返回用户的详细信息。

## 12、设置自定义元数据
#### 请求 URL
```
PATCH http://192.168.5.100/artifactory/api/storage/:repositoryId/:artifactPath
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| customProperties | query | string | 是 | 自定义元数据信息 |

#### 示例请求
```
PATCH http://192.168.5.100/artifactory/api/storage/local-raw/demo.tar.gz?customProperties=key=value
```

#### 响应参数
返回操作结果信息。

## 13、删除制品或目录
#### 请求 URL
```
DELETE http://192.168.5.100/artifactory/api/storage/:repositoryId/:artifactPath
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| force | query | string | 否 | 是否强制删除 |

#### 示例请求
```
DELETE http://192.168.5.100/artifactory/api/storage/local-raw/demo.tar.gz?force=true
```

#### 响应参数
返回删除结果信息。

## 14、递归获取该目录下的制品或者文件
#### 请求 URL
```
GET http:///artifactory/api/storage/:repositoryId/:artifactPath
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| list | query | boolean | 是 | 是否列出目录内容 |
| deep | query | integer | 是 | 递归深度，设置为 1 表示递归获取 |

#### 示例请求
```
GET http:///artifactory/api/storage/local-raw/demo-dir?list&deep=1
```

#### 响应参数
返回目录下的制品或文件信息列表。

## 15、Pattern 搜索
#### 请求 URL
```
GET http://127.0.0.1:38080/api/search/pattern
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| pattern | query | string | 是 | 搜索模式，格式为 {repositoryId}:{path} |

#### 示例请求
```
GET http://192.168.5.100:38080/artifactory/api/search/pattern?pattern=local-raw:66/*.har
```

#### 响应参数
返回符合搜索模式的制品信息列表。

## 16、移动接口
#### 请求 URL
```
POST http://127.0.0.1:38080/artifactory/api/move/{srcRepoKey}/{srcFilePath}
```

#### 请求参数
| 参数名 | 位置 | 类型 | 是否必选 | 描述 |
| ---- | ---- | ---- | ---- | ---- |
| to | query | string | 是 | 目标仓库和文件路径，格式为 /{targetRepoKey}/{targetFilePath} |

#### 示例请求
```
POST http://192.168.5.100:38080/artifactory/api/move/local-raw/01-16?to=local-raw-sit/01-16
```

#### 响应参数
返回移动操作结果信息。
