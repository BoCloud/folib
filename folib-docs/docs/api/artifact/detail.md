# GET 获取制品信息

`GET /api/browse/getArtifact/{storageId}/{repositoryId}/{artifactPath}`

## Body 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|storageId|path|string| 是 ||存储空间id|
|repositoryId|path|string| 是 ||仓库id|
|artifactPath|path|string| 是 ||制品路径|
|Content-Type|header|string| 是 ||application/json|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
{
  "artifact": {
    "uuid": "demo-project-demo-local-raw-license.zip",
    "storageId": "demo-project",
    "repositoryId": "demo-local-raw",
    "storageIdAndRepositoryId": "demo-project-demo-local-raw",
    "checksums": {
      "SHA-1": "5d08e4123d210af129454f3b44273d9c4fa67c55",
      "SM3": "ddaa44a177a7ce9867a30eb267cc35e0887c251647fe999fabd851d69318f505",
      "SHA-256": "c804d95e39c56d9da4c8647eb23414f5e850130869f074445cee852fe1950512",
      "MD5": "f789e8da9c3eb72751f456faef295109"
    },
    "sizeInBytes": 4368,
    "downloadCount": 0,
    "artifactArchiveListing": {
      "filenames": [
        "__MACOSX/._6aba-762a-b4fd-f414-4d05-2a0e-a556-34d5-folib.lic",
        "e528-f514-6f87-acfe-6e3e-2534-cfa2-5e1f-folib.lic",
        "__MACOSX/._6dc3-f3a2-09cd-04d3-2c0f-5b63-60a9-d5fd-folib.lic",
        "e0be-b1b2-26c8-2614-8cb1-9281-723d-4b4d-folib.lic",
        "6dc3-f3a2-09cd-04d3-2c0f-5b63-60a9-d5fd-folib.lic",
        "__MACOSX/._e0be-b1b2-26c8-2614-8cb1-9281-723d-4b4d-folib.lic",
        "__MACOSX/._e528-f514-6f87-acfe-6e3e-2534-cfa2-5e1f-folib.lic",
        "6aba-762a-b4fd-f414-4d05-2a0e-a556-34d5-folib.lic"
      ]
    },
    "artifactFileExists": true,
    "safeLevel": "unScan",
    "evidenceQuantity": 0,
    "dependencyCount": 0,
    "dependencyVulnerabilitiesCount": 0,
    "vulnerabilitiesCount": 0,
    "criticalVulnerabilitiesCount": 0,
    "highVulnerabilitiesCount": 0,
    "mediumVulnerabilitiesCount": 0,
    "lowVulnerabilitiesCount": 0,
    "suppressedVulnerabilitiesCount": 0,
    "vulnerabilitySet": [],
    "metadata": "{\"ID\":{\"value\":\"1111111\",\"type\":\"STRING\",\"viewShow\":1}}",
    "scanDate": null,
    "scanDateTime": null,
    "report": null,
    "createdBy": "admin",
    "updatedBy": "admin",
    "artifactName": "license.zip",
    "artifactPath": "license.zip"
  },
  "createdTime": "2024-09-10 17:28:02",
  "listTree": [
    {
      "name": "__MACOSX",
      "type": "DIR",
      "isLeaf": false,
      "children": [
        {
          "name": "._6aba-762a-b4fd-f414-4d05-2a0e-a556-34d5-folib.lic",
          "type": "FILE",
          "isLeaf": true,
          "children": null
        },
        {
          "name": "._6dc3-f3a2-09cd-04d3-2c0f-5b63-60a9-d5fd-folib.lic",
          "type": "FILE",
          "isLeaf": true,
          "children": null
        },
        {
          "name": "._e0be-b1b2-26c8-2614-8cb1-9281-723d-4b4d-folib.lic",
          "type": "FILE",
          "isLeaf": true,
          "children": null
        },
        {
          "name": "._e528-f514-6f87-acfe-6e3e-2534-cfa2-5e1f-folib.lic",
          "type": "FILE",
          "isLeaf": true,
          "children": null
        }
      ]
    },
    {
      "name": "e528-f514-6f87-acfe-6e3e-2534-cfa2-5e1f-folib.lic",
      "type": "FILE",
      "isLeaf": true,
      "children": null
    },
    {
      "name": "e0be-b1b2-26c8-2614-8cb1-9281-723d-4b4d-folib.lic",
      "type": "FILE",
      "isLeaf": true,
      "children": null
    },
    {
      "name": "6dc3-f3a2-09cd-04d3-2c0f-5b63-60a9-d5fd-folib.lic",
      "type": "FILE",
      "isLeaf": true,
      "children": null
    },
    {
      "name": "6aba-762a-b4fd-f414-4d05-2a0e-a556-34d5-folib.lic",
      "type": "FILE",
      "isLeaf": true,
      "children": null
    }
  ],
  "lastModified": "2024-09-10 18:05:01",
  "lastUsedTime": "2024-09-10 18:05:01",
  "sha": "5d08e4123d210af129454f3b44273d9c4fa67c55",
  "downloadCount": 0,
  "md5": "f789e8da9c3eb72751f456faef295109"
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
|artifact|object|true|none|制品信息|none|
|uuid|string|true|none|制品id|none|
|storageId|string|true|none|存储空间id|none|
|repositoryId|string|true|none|仓库id|none|
|storageIdAndRepositoryId|string|true|none|存储空间-仓库组合id|none|
|checksums|object|true|none|校验码信息|none|
|SHA-1|string|true|none||none|
|SM3|string|true|none||none|
|SHA-256|string|true|none||none|
|MD5|string|true|none||none|
|sizeInBytes|integer|true|none|制品大小|单位是bytes|
|downloadCount|integer|true|none|下载次数|none|
|artifactArchiveListing|object|false|none|制品压缩包信息|none|
|filenames|[string]|true|none|文件名列表|none|
|safeLevel|string|true|none|扫描状态|扫描状态<br /> unScan（未扫描）<br />scanning（扫描中）<br />scanComplete（扫描完成）<br />scanFail（扫描失败）<br />unwantedScan（无需扫描）|
|evidenceQuantity|integer|true|none|风险凭证个数|none|
|dependencyCount|integer|true|none|依赖数量|none|
|dependencyVulnerabilitiesCount|integer|true|none|有漏洞的依赖数量|none|
|vulnerabilitiesCount|integer|true|none|漏洞总数|none|
|criticalVulnerabilitiesCount|integer|true|none|严重漏洞数量|none|
|highVulnerabilitiesCount|integer|true|none|高危漏洞数量|none|
|mediumVulnerabilitiesCount|integer|true|none|中危漏洞数量|none|
|lowVulnerabilitiesCount|integer|true|none|低危漏洞数量|none|
|suppressedVulnerabilitiesCount|integer|true|none|被封存的漏洞数量|none|
|vulnerabilitySet|[string]|false|none|漏洞编号列表|none|
|metadata|string|false|none|元数据|none|
|scanDate|string|false|none|扫描日期|none|
|scanDateTime|string|false|none|扫描时间|none|
|report|string|false|none|扫描报告|none|
|createdBy|string|true|none|创建人|none|
|updatedBy|string|true|none|更新人|none|
|artifactName|string|true|none|制品名称|none|
|artifactPath|string|true|none|制品路径|none|
|createdTime|string|true|none|创建时间|none|
|listTree|[object]|false|none|制品包内结构信息|none|
|name|string|true|none|名称|none|
|type|string|true|none|类型|DIR （文件夹）FILE（文件）|
|isLeaf|boolean|true|none|叶子节点|true 是 false 不是|
|children|[object]|false|none||子节点列表|
|name|string|true|none|名称|none|
|type|string|true|none|类型|DIR （文件夹）FILE（文件）|
|isLeaf|boolean|true|none|叶子节点|true 是 false 不是|
|lastModified|string|true|none|最后更新时间|none|
|lastUsedTime|string|true|none|最近使用时间|none|
|sha|string|true|none|sha1校验码|none|
|downloadCount|integer|true|none|下载次数|none|
|md5|string|true|none|md5校验码|none|
