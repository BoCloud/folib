package com.veadan.folib.controllers.layout.docker;


import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.controllers.BaseArtifactController;

import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.providers.io.RepositoryPath;

import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.utils.FileUtils;
import io.swagger.annotations.ApiOperation;

import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.MappedByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;


/**
 * REST API for all artifact-related processes.
 * <p>
 * Thanks to custom URL processing any path variable like '{artifactPath:.+}' will be processed as '**'.
 *
 * @author Martin Todorov
 * @author Alex Oreshkevich
 * @author Przemyslaw Fusik
 * @author Sergey Bespalov
 * @see{@linkplain http://docs.spring.io/spring/docs/current/spring-framework-reference/html/mvc.html#mvc-config-path-matching}
 */
@RestController
public class DockerArtifactController
        extends BaseArtifactController {

    final ConcurrentHashMap<String, Long> ranges = new ConcurrentHashMap<String, Long>();

    ConcurrentHashMap<String, String> data = new ConcurrentHashMap<String, String>();

    /**
     * 认证
     *
     *
     * @param request
     * @param response
     * @return
     * @throws Exception
     */
    @RequestMapping(value = {"/v2/"}, method = {RequestMethod.GET})
    public ResponseEntity<String> checkRepositoryAccess(@RequestHeader(HttpHeaders.AUTHORIZATION) String Authorization,
                                                        HttpServletRequest request,
                                                        HttpServletResponse response)
            throws Exception {
        System.out.printf(request.toString());
        System.out.printf(Authorization);
        return new ResponseEntity<>("success", HttpStatus.OK);
    }


    /**
     * Starting An Upload 开始上传
     *
     * @param httpHeaders
     * @param request
     * @param response
     */
    //TODO {artifactPath:.+}
    @ApiOperation(value = "tarting An Upload 开始上传")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/uploads/"}, method = {RequestMethod.POST}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> startingAnUpload(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageName,
                                                   @PathVariable String repName,
                                                   @PathVariable String name

    ) {
        final String storageId = storageName;
        final String repositoryId = repName;
        final String path = name;


        try {
            String contentType = request.getContentType();
            String uuid = UUID.randomUUID().toString();
            String url = new StringBuffer().append(request.getRequestURI()).append(uuid).toString();
            response.reset();
            response.setDateHeader("Date", System.currentTimeMillis());
            response.setHeader("Docker-Distribution-Api-Version", "registry/2.0");
            response.setHeader("Docker-Upload-UUID", uuid);
            response.setHeader("Location", url);
            response.setHeader("range", "bytes=0--1");
            response.setHeader("Content-Length", "0");

            return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

    //GET /v2/<name>/blobs/uploads/<uuid>
    //

    @ApiOperation(value = "Upload Progress 上传进度")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.GET}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> uploadProgress(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageName,
                                                 @PathVariable String repName,
                                                 @PathVariable String name,
                                                 @PathVariable String uuid

    ) {

        response.addHeader("Range", ranges.get(uuid).toString());
        return new ResponseEntity<>("OK", HttpStatus.NO_CONTENT);
    }

    @ApiOperation(value = "Monolithic Upload 单片上传")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.PUT}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> monolithicUpload(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageName,
                                                   @PathVariable String repName,
                                                   @PathVariable String name,
                                                   @PathVariable String uuid,
                                                   @RequestParam String digest

    ) {
        final String storageId = storageName;
        final String repositoryId = repName;
        final String path = repName + "/" + name + "/" + digest;
        try {
            String contentType = request.getContentType();
            int totalBytes = request.getContentLength();
            // TODO totalBytes > 0
            //InputStream inputStream = request.getInputStream();
            if (totalBytes == 0) {
                //inputStream = utils.getFile(fileDir,fileName );
                totalBytes = ranges.get(uuid).intValue();
                if (data.containsKey(uuid)) {
                    data.replace(digest, uuid);
                } else {
                    data.put(digest, uuid);
                }
            }

            String url = new StringBuffer().append("http://").append(request.getRemoteHost()).append(request.getRequestURI()).toString();
            url = url.replace(uuid, digest);

            //Docker-Content-Digest	sha256:0e28711eb56d78f1e3dfde1807eba529d1346222bcd07d1cb1e436a18a0388bd
            response.reset();
            response.setDateHeader("Date", System.currentTimeMillis());
            response.setHeader("Docker-Distribution-Api-Version", "registry/2.0");
            response.setHeader("Location", url);
            response.setHeader("Docker-Content-Digest", digest);
            response.setHeader("Content-Range", "0-" + totalBytes);
            //Content-Range	0-19778034
            //202 Accepted
            return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        } finally {
            // utils.deleteDir( fileDir,fileName);
        }

    }


    @ApiOperation(value = "Chunked Upload 分片上传")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.PATCH}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> chunkedUpload(@RequestHeader HttpHeaders httpHeaders,
                                                HttpServletRequest request,
                                                HttpServletResponse response,
                                                @PathVariable String storageName,
                                                @PathVariable String repName,
                                                @PathVariable String name,
                                                @PathVariable String uuid,
                                                @RequestBody String inputStream


    ) throws Exception {
        final String storageId = storageName;
        final String repositoryId = repName;
        final String path = name + "/" + uuid;
        response.setCharacterEncoding("utf8");
        //Content-Length: <size of chunk>
        //Content-Range: <start of range>-<end of range>
        //Content-Type: application/octet-stream

        String contentRange = request.getHeader("Content-Range");
        String range = request.getHeader("Range");
        int contentLength = request.getContentLength();
        byte[] bytes = inputStream.getBytes();

        String contentType = request.getContentType();
        int totalBytes = request.getContentLength();
        FileUtils utils = new FileUtils();
        String fileDir = new StringBuffer().append(storageId).append("/").append(repName).append("/").append(name).toString();
        String fileName = uuid;
        utils.upload(fileDir, fileName, 0, bytes);

        if (ranges.containsKey(uuid)) {
            ranges.replace(uuid, ranges.get(uuid) + bytes.length);
        } else {
            ranges.put(uuid, (long) bytes.length);
        }
//        InputStream input = new ByteArrayInputStream(bytes);
//        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
//        artifactManagementService.validateAndStore(repositoryPath, dataInputStream);

        String url = request.getRequestURI();
        response.reset();
        response.setDateHeader("Date", System.currentTimeMillis());
        response.setHeader("Docker-Distribution-Api-Version", "registry/2.0");
        response.setHeader("Docker-Upload-UUID", uuid);
        response.setHeader("Location", url);
        response.setHeader("Range", "0-" + ranges.get(uuid).intValue());
        response.setHeader("Content-Length", "0");
        //202 Accepted
        return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
    }


    //http://192.168.1.6:8082/v2/repository/test-docker/node/blobs/uploads/9eb1b9a2-af53-4ec5-aeec-c557b92dab65?digest=sha256:e460dd483fddb555911f7ed188c319fd97542c60e36843dcb1c5d753f733e1fa

    @ApiOperation(value = "Uploading the Layer 上传图层")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.POST}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> uploadingTheLayer(@RequestHeader HttpHeaders httpHeaders,
                                                    HttpServletRequest request,
                                                    HttpServletResponse response,
                                                    @PathVariable String storageName,
                                                    @PathVariable String repName,
                                                    @PathVariable String name

    ) {
        final String storageId = storageName;
        final String repositoryId = repName;
        final String path = name;


        try {
            String contentType = request.getContentType();
            BufferedReader reader = request.getReader();
            int totalBytes = request.getContentLength();
            InputStream inputStream = request.getInputStream();
            DataInputStream dataInputStream = new DataInputStream(inputStream);
            byte[] bytes = new byte[totalBytes];
            dataInputStream.readFully(bytes);
            dataInputStream.close();
            inputStream = new ByteArrayInputStream(bytes);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, inputStream);
            //202 Accepted
            return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    /**
     * Existing Layers 现有层
     *
     * @param httpHeaders
     * @param request
     * @param response
     */
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/{digest}"}, method = {RequestMethod.HEAD}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity existingLayers(@RequestHeader HttpHeaders httpHeaders,
                                         HttpServletRequest request,
                                         HttpServletResponse response,
                                         @PathVariable String storageName,
                                         @PathVariable String repName,
                                         @PathVariable String name,
                                         @PathVariable String digest
    ) {
        final String storageId = storageName;
        final String repositoryId = repName;
        final String artifactPath = name + "/" + digest;

        try {
            //todo可以通过对 blob 存储 API 的请求来检查层是否存在。请求的格式应如下所示：HEAD
            // HEAD /v2/<name>/blobs/<digest>
            //Content-Length: <length of blob>
            //        Docker-Content-Digest: <digest>

            response.reset();
            //todo 200已经存在 404不存在
            if (data.containsKey(digest)) {

                response.addHeader("Docker-Distribution-Api-Version", "registry/2.0");
                response.addHeader("Accept-Ranges", "bytes");
                response.addHeader("Docker-Content-Digest", digest);
                return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
            } else {
                response.addHeader("Docker-Distribution-Api-Version", "registry/2.0");
                response.addHeader("Accept-Ranges", "bytes");
                response.addHeader("Docker-Content-Digest", digest);
                return new ResponseEntity<>("OK", HttpStatus.NOT_FOUND);
            }

        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }


    /**
     * Uploading the Layer 上传进度
     *
     * @param httpHeaders
     * @param request
     * @param response
     * @param uuid
     */
    @RequestMapping(value = {"/v2/{storageName}/{repName}/blobs/uploads/{uuid}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity uploadingTheLayer(@RequestHeader HttpHeaders httpHeaders,
                                            HttpServletRequest request,
                                            HttpServletResponse response,
                                            @PathVariable String storageName,
                                            @PathVariable String repName,
                                            @PathVariable String name,
                                            @PathVariable String uuid
    ) {
        //GET /v2/<name>/blobs/uploads/<uuid>
        //204 No Content
        //Location: /v2/<name>/blobs/uploads/<uuid>
        //Range: bytes=0-<offset>
        //Docker-Upload-UUID: <uuid>
        FileUtils utils = new FileUtils();
        String fileDir = new StringBuffer().append(storageName).append("/").append(repName).append("/").append(name).toString();
        String fileName = uuid;
        long range = utils.getOffset(fileDir, fileName);
        response.reset();
        response.setDateHeader("Date", System.currentTimeMillis());
        response.addHeader("Docker-Distribution-Api-Version", "registry/2.0");
        response.addHeader("Range", "bytes=0-" + range);
        response.addHeader("Location", request.getRequestURI());
        response.addHeader("Docker-Upload-UUID", uuid);
        return new ResponseEntity("", HttpStatus.NO_CONTENT);
    }


    /**
     * Pushing an Image Manifest
     *
     * @param httpHeaders
     * @param request
     * @param response
     * @param storageId
     * @param repositoryId
     * @param name
     * @param tag
     */
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{tag}"}, method = {RequestMethod.PUT})
    public ResponseEntity pushingAnImageManifest(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String tag,
                                                 @RequestBody JSONObject json
    ) {

        final String artifactPath = name + "/" + tag + "/manifest.json";

        try {
            int totalBytes = request.getContentLength();
            System.out.println("当前数据总长度:" + totalBytes);
            // InputStream stream = new ByteArrayInputStream(json.toString().getBytes(StandardCharsets.UTF_8));
            String contentType = request.getContentType();
            ImageManifest manifest = json.toJavaObject(ImageManifest.class);
            //if(!Objects.equals("application/vnd.docker.distribution.manifest.v2+json",contentType)){

            // }else {
//            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
//            artifactManagementService.validateAndStore(repositoryPath, stream);
            // }
            //stream.close();
            imagesStorage(storageId,
                    repositoryId,
                    name,
                    tag,
                    json);
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    //镜像存储

    Boolean imagesStorage(String storageId,
                          String repositoryId,
                          String name,
                          String tag,
                          JSONObject json) {
        InputStream stream = new ByteArrayInputStream(json.toString().getBytes(StandardCharsets.UTF_8));
        ImageManifest manifest = json.toJavaObject(ImageManifest.class);


        try {
            for (LayerManifest item : manifest.getLayers()) {

                InputStream inputStream = storageData(storageId, repositoryId, name, item.getDigest());
                String artifactPath = new StringBuffer().append(name).append("/").append(tag).append("/").append(item.getDigest()).toString();
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                artifactManagementService.validateAndStore(repositoryPath, inputStream);
            }

            String artifactPath = new StringBuffer().append(name)
                    .append("/")
                    .append(tag)
                    .append("manifest.json")
                    .toString();
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, stream);

        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        } finally {
            deleteLayers(storageId,
                    repositoryId,
                    name,
                    manifest.getLayers().stream().map(item -> item.getDigest()).collect(Collectors.toList()));
        }
        return true;
    }

    void deleteLayers(String storageId,
                      String repositoryId,
                      String name,
                      List<String> digests) {
        FileUtils utils = new FileUtils();
        String fileDir = new StringBuffer().append(storageId)
                .append("/")
                .append(repositoryId)
                .append("/")
                .append(name)
                .toString();
        digests.forEach(item -> {
            String fileName = data.get(item);
            if (data.containsKey(item) && Objects.nonNull(data.get(item))) {
                utils.deleteDir(fileDir, fileName);
                data.remove(item);
            }
        });

    }

    InputStream storageData(String storageId,
                            String repositoryId,
                            String name,
                            String digest) {
        FileUtils utils = new FileUtils();
        String fileDir = new StringBuffer().append(storageId)
                .append("/")
                .append(repositoryId)
                .append("/")
                .append(name)
                .toString();
        String fileName = data.get(digest);

        return utils.getFile(fileDir, fileName);
    }


    /**
     * 获取镜像单
     *
     * @param httpHeaders
     * @param request
     * @param response
     * @param repName
     * @param name
     * @param tag
     */
    @RequestMapping(value = {"/v2/{repName}/{name}/manifests/{tag}"}, method = {RequestMethod.GET})
    public void getManifest(@RequestHeader HttpHeaders httpHeaders,
                            HttpServletRequest request,
                            HttpServletResponse response,
                            @PathVariable String repName,
                            @PathVariable String name,
                            @PathVariable String tag
    ) {
        System.out.println("recv ntydel from:" + request.getRequestURI());
        Map<String, String> data = new HashMap<>();
        data.put("access_token", "11111111111111");
        data.put("token", "111111111111111");


        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
        new ResponseEntity<>(data, HttpStatus.OK);
    }


}

