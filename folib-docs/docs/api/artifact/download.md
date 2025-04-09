# GET 下载制品

`GET /storages/{storageId}/{repositoryId}/{artifactPath}`

适用于

+ Raw仓库
+ Maven仓库
+ Ivy仓库
+ SBT仓库
+ Gradle 仓库
+ Rpm仓库
+ Php仓库
+ Helm仓库
+ CocoaPods仓库
+ Go仓库
+ Git LFS仓库
+ Huggingface仓库
+ Debian仓库

## Body 请求参数

> 参数表

|名称|位置|类型|必选|中文名|说明|
|:---:|:---:|:---:|:---:|:---:|:---:|
|storageId|path|string| 是 ||存储空间id|
|repositoryId|path|string| 是 ||仓库id|
|artifactPath|path|string| 是 ||制品路径，仓库下的相对路径|
|Authorization|header|string| 否 ||认证信息|

## 详细说明

**Authorization**: 认证信息

`Bearer eyJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJGb2xpYiIsImp0aSI6IjRhYTM4OTViM2MwZDRjNjZhNjMwM2JhOWI1MTEzNDZmIiwic3ViIjoiYWRtaW4iLCJ1c2VySGFzaCI6Ii01MDc5MTM4MjYiLCJwYWMiOiIxIiwiaWF0IjoxNzI1OTUwMjg4LCJleHAiOjE3MjY1NTUwODh9.S1EDLz8QxF-AsFuT05DnDb98qADF0TKX5KiglBCIEYc`

> 返回示例

```shell
200 Response
```

## 返回结果

|状态码|状态码含义|说明|数据模型|
|:---:|:---:|:---:|:---:|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
