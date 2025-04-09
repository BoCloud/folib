# GET 搜索制品

`GET /api/fql`

## Body 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|artifactName|query|string| 否 ||制品名称或者正则表达式|
|storageId|query|string| 否 ||存储空间id|
|repositoryId|query|string| 否 ||仓库id|
|limit|query|string| 否 ||每页条数|
|page|query|string| 否 ||页码|
|regex|query|boolean| 否 ||正则设定|
|beginDate|query|string| 否 ||开始时间|
|endDate|query|string| 否 ||结束时间|
|metadataSearch|query|string| 否 ||元数据搜索|
|repositoryIds|query|array[string]| 否 ||仓库列表|
|sortField|query|string| 否 ||排序字段|
|sortOrder|query|string| 否 ||排序顺序|
|digestAlgorithm|query|string| 否 ||校验码类型|
|digest|query|string| 否 ||校验码|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**regex**: 正则设定

+ true （启用正则）
+ 其他 （不启用正则）

**sortField**: 排序字段

+ created (创建时间)
+ lastUsed（最近使用时间）
+ downloadCount（下载次数）
+ sizeInBytes（制品大小）

**sortOrder**: 排序顺序

+ asc（升序）
+ desc（降序）

**digestAlgorithm**: 校验码类型

+ MD5
+ SHA-1
+ SHA-256
+ SHA-512
+ SM3

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "total": 2,
  "artifact": [
    {
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw",
      "url": "http://192.168.5.8:38080/storages/demo-project/demo-local-raw/license.zip",
      "checksums": {
        "SHA-1": "5d08e4123d210af129454f3b44273d9c4fa67c55",
        "SM3": "ddaa44a177a7ce9867a30eb267cc35e0887c251647fe999fabd851d69318f505",
        "SHA-256": "c804d95e39c56d9da4c8647eb23414f5e850130869f074445cee852fe1950512",
        "MD5": "f789e8da9c3eb72751f456faef295109"
      },
      "sizeInBytes": 4368,
      "lastUpdated": "2024-09-10 17:28:21",
      "lastUsed": "2024-09-10 17:28:21",
      "created": "2024-09-10 17:28:02",
      "sha": "5d08e4123d210af129454f3b44273d9c4fa67c55",
      "md5": "f789e8da9c3eb72751f456faef295109",
      "artifactName": "license.zip",
      "artifactPath": "license.zip",
      "layout": "Raw",
      "subLayout": "raw",
      "path": "license.zip",
      "vulnerabilitiesCount": 0,
      "criticalVulnerabilitiesCount": 0,
      "highVulnerabilitiesCount": 0,
      "mediumVulnerabilitiesCount": 0,
      "lowVulnerabilitiesCount": 0,
      "suppressedVulnerabilitiesCount": 0,
      "downloadCount": 0
    },
    {
      "storageId": "demo-project",
      "repositoryId": "demo-local-raw",
      "url": "http://192.168.5.8:38080/storages/demo-project/demo-local-raw/09-10/license.zip",
      "checksums": {
        "SHA-1": "5d08e4123d210af129454f3b44273d9c4fa67c55",
        "SM3": "ddaa44a177a7ce9867a30eb267cc35e0887c251647fe999fabd851d69318f505",
        "SHA-256": "c804d95e39c56d9da4c8647eb23414f5e850130869f074445cee852fe1950512",
        "MD5": "f789e8da9c3eb72751f456faef295109"
      },
      "sizeInBytes": 4368,
      "lastUpdated": "2024-09-10 17:28:08",
      "lastUsed": "2024-09-10 17:28:08",
      "created": "2024-09-10 17:27:56",
      "sha": "5d08e4123d210af129454f3b44273d9c4fa67c55",
      "md5": "f789e8da9c3eb72751f456faef295109",
      "artifactName": "license.zip",
      "artifactPath": "09-10/license.zip",
      "layout": "Raw",
      "subLayout": "raw",
      "path": "09-10/license.zip",
      "vulnerabilitiesCount": 0,
      "criticalVulnerabilitiesCount": 0,
      "highVulnerabilitiesCount": 0,
      "mediumVulnerabilitiesCount": 0,
      "lowVulnerabilitiesCount": 0,
      "suppressedVulnerabilitiesCount": 0,
      "downloadCount": 0
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
|total|integer|true|none|总数|none|
|artifact|[object]|true|none|制品列表|none|
|storageId|string|true|none|存储空间id|none|
|repositoryId|string|true|none|仓库id|none|
|url|string|true|none|下载地址|none|
|checksums|object|true|none|校验码信息|none|
|SHA-1|string|true|none||none|
|SM3|string|true|none||none|
|SHA-256|string|true|none||none|
|MD5|string|true|none||none|
|sizeInBytes|integer|true|none|制品大小|单位是bytes|
|lastUpdated|string|true|none|最后更新时间|none|
|lastUsed|string|true|none|最近使用时间|none|
|created|string|true|none|创建时间|none|
|sha|string|true|none|sha1校验码|none|
|md5|string|true|none|md5校验码|none|
|artifactName|string|true|none|制品名称|none|
|artifactPath|string|true|none|制品路径|none|
|layout|string|true|none|仓库包类型|none|
|subLayout|string|true|none|仓库包子类型|none|
|path|string|true|none|制品路径|none|
|vulnerabilitiesCount|integer|true|none|漏洞总数|none|
|criticalVulnerabilitiesCount|integer|true|none|严重漏洞数量|none|
|highVulnerabilitiesCount|integer|true|none|高危漏洞数量|none|
|mediumVulnerabilitiesCount|integer|true|none|中危漏洞数量|none|
|lowVulnerabilitiesCount|integer|true|none|低危漏洞数量|none|
|suppressedVulnerabilitiesCount|integer|true|none|被封存的漏洞数量|none|
|downloadCount|integer|true|none|下载次数|none|
