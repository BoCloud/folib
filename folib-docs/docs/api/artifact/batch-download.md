# POST 制品批量下载获取路径

`POST /artifactory/resolveBatchPath`

## Body 请求参数

```json
{
  "files": [
    {
      "pattern": "artifactory/public/upload/brcc2tar.gz",
      "target": "/root/bin/linux_amd64/test",
      "recursive": false,
      "flat": false
    }
  ]
}
```

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|Content-Type|header|string| 是 ||multipart/form-data|
|Accept|header|string| 是 ||application/json, text/plain|
|Authorization|header|string| 是 ||认证信息|
|body|body|object| 否 ||none|
|» files|body|[object]| 是 ||none|
|»» pattern|body|string| 是 | 仓库下的路径|存储空间开头的路径|
|»» target|body|string| 是 | 下载存放路径|文件下载到本地的路径|
|»» recursive|body|boolean| 否 | 是否递归子目录|是否递归子目录，true递归子目录，false不递归子目录。默认为true|
|»» flat|body|boolean| 否 | 是否创建子目录|是否创建子目录，true创建子目录，false不创建子目录。默认为false|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```json
[
  {
    "url": "https://demo2.folib.com/artifactory/public/upload/brcc2tar.gz",
    "target": "/root/bin/linux_amd64/test/brcc2tar.gz"
  }
]
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|

> 返回数据结构

状态码 **200**

|名称|类型|必选|约束|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|url|string|true|none|下载链接|单个制品的下载链接|
|target|string|true|none|下载存放路径|文件下载到本地的路径|
