package com.veadan.folib.controllers.layout.docker;


import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.controllers.BaseArtifactController;

import com.veadan.folib.providers.io.RepositoryPath;

import io.swagger.annotations.ApiOperation;

import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import liquibase.pro.packaged.S;
import org.apache.commons.codec.cli.Digest;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FileUtils;
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
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;


import static org.springframework.http.HttpStatus.NOT_FOUND;

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

    ConcurrentHashMap<String, Long> ranges = new ConcurrentHashMap<String, Long>();

    ConcurrentHashMap<String, String> data = new ConcurrentHashMap<String, String>();
    /**
     * 认证
     *
     * @param httpHeaders
     * @param request
     * @param response
     * @return
     * @throws Exception
     */
    @RequestMapping(value = {"/v2/"}, method = {RequestMethod.GET})
    public ResponseEntity<String> checkRepositoryAccess(@RequestHeader HttpHeaders httpHeaders,
                                                        HttpServletRequest request,
                                                        HttpServletResponse response)
            throws Exception {
        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
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
            String  uuid = UUID.randomUUID().toString();
            String url = new  StringBuffer().append(request.getRequestURI()).append(uuid).toString();
            response.reset();
            response.setDateHeader("Date",System.currentTimeMillis());
            response.setHeader("Docker-Distribution-Api-Version","registry/2.0");
            response.setHeader("Docker-Upload-UUID",uuid);
            response.setHeader("Location",url);
            response.setHeader("range","bytes=0--1");
            response.setHeader("Content-Length","0");

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
        final String path = name + "/" + digest;

        try {
            String contentType = request.getContentType();
            int totalBytes = request.getContentLength();
            InputStream inputStream = request.getInputStream();
            DataInputStream dataInputStream = new DataInputStream(inputStream);
            DataInputStream dataInputStream2 = new DataInputStream(dataInputStream);
            byte[] bytes = new byte[totalBytes];
            byte[] bytes2 = new byte[1024];
            dataInputStream.readFully(bytes);
            if(ranges.containsKey(uuid)){
                ranges.remove(uuid);
            }
            AtomicLong red = new AtomicLong(1);
            if(totalBytes>0){
                while (red.addAndGet(dataInputStream2.read(bytes2)) < totalBytes) {
                    if(ranges.containsKey(uuid)){
                        long redData = ranges.get(uuid);
                        redData =redData+red.longValue();
                        ranges.replace(uuid,redData);
                    }else {
                        ranges.put(uuid, red.longValue());
                    }
                }
            }
            dataInputStream2.close();
            dataInputStream.close();
            inputStream = new ByteArrayInputStream(bytes);

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
            artifactManagementService.validateAndStore(repositoryPath, inputStream);
            String url = new  StringBuffer().append("http://")
                    .append(request.getRemoteHost()).append(request.getRequestURI()).toString();
             url =url.replace(uuid,digest);

            //Docker-Content-Digest	sha256:0e28711eb56d78f1e3dfde1807eba529d1346222bcd07d1cb1e436a18a0388bd
            response.reset();
            response.setDateHeader("Date",System.currentTimeMillis());
            response.setHeader("Docker-Distribution-Api-Version","registry/2.0");
            response.setHeader("Location",url);
            response.setHeader("Docker-Content-Digest",digest);
            response.setHeader("Content-Range","0-"+totalBytes);
            //Content-Range	0-19778034
            //202 Accepted
            return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }

//    ConcurrentHashMap<String, AtomicLong> chunks = new ConcurrentHashMap<String, AtomicLong>();
//    public void atest(Long chunkSizes, byte[] fileData,String digest){
//
//        RandomAccessFile tempRaf = null;
//        FileChannel fileChannel = null;
//        MappedByteBuffer mappedByteBuffer = null;
//        try {
//            String uploadDirPath = filePathUtil.getPath(param);
//            File tmpFile = super.createTmpFile(param);
//            tempRaf = new RandomAccessFile(tmpFile, "rw");
//            fileChannel = tempRaf.getChannel();
//            long chunk = fileData.length;
//            //位置
//            if(chunks.containsKey(digest)){
//                chunk= chunks.get(digest).addAndGet(fileData.length);
//            }else {
//                chunks.put(digest,new AtomicLong(fileData.length));
//            }
//            long chunkSize =chunkSizes;
//            //写入该分片数据
//            long offset = chunk;
//
//            mappedByteBuffer = fileChannel
//                    .map(FileChannel.MapMode.READ_WRITE, offset, fileData.length);
//            mappedByteBuffer.put(fileData);
//
//
//        } catch (FileNotFoundException e) {
//            e.printStackTrace();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//    }

    /**
     * 检查并修改文件上传进度
     */
//    public void checkAndSetUploadProgress(String digest, String uploadDirPath) {
//
//        String fileName = digest;
//        File confFile = new File(uploadDirPath, fileName + ".conf");
//        byte isComplete = 0;
//        RandomAccessFile accessConfFile = null;
//        try {
//            accessConfFile = new RandomAccessFile(confFile, "rw");
//            //把该分段标记为 true 表示完成
//            System.out.println("set part " + param.getChunk() + " complete");
//            //创建conf文件文件长度为总分片数，每上传一个分块即向conf文件中写入一个127，那么没上传的位置就是默认0,已上传的就是Byte.MAX_VALUE 127
//            accessConfFile.setLength(param.getChunks());
//            accessConfFile.seek(param.getChunk());
//            accessConfFile.write(Byte.MAX_VALUE);
//
//            //completeList 检查是否全部完成,如果数组里是否全部都是127(全部分片都成功上传)
//            byte[] completeList = FileUtils.readFileToByteArray(confFile);
//            isComplete = Byte.MAX_VALUE;
//            for (int i = 0; i < completeList.length && isComplete == Byte.MAX_VALUE; i++) {
//                //与运算, 如果有部分没有完成则 isComplete 不是 Byte.MAX_VALUE
//                isComplete = (byte) (isComplete & completeList[i]);
//                System.out.println("check part " + i + " complete?:" + completeList[i]);
//            }
//
//        } catch (IOException e) {
//            e.getMessage();
//        } finally {
//            try {
//                accessConfFile.close();
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }
//    }

    @ApiOperation(value = "Chunked Upload 分片上传")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
                           @ApiResponse(code = 400, message = "An error occurred.")})
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.PATCH}, consumes = MediaType.ALL_VALUE)
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
        //Content-Range
        String a = request.getContextPath();
        String b = request.getHeader("Content-Range");
        String c = request.getHeader("Range");
        byte[] bytes =  inputStream.getBytes();

        String contentType = request.getContentType();
        int totalBytes = request.getContentLength();

        DataInputStream dataInputStream = new DataInputStream(new ByteArrayInputStream(bytes));

        byte[] bytes2 = new byte[1024];
        dataInputStream.readFully(bytes);
        if(ranges.containsKey(uuid)){
            ranges.remove(uuid);
        }
        AtomicLong red = new AtomicLong(1);
        while (red.addAndGet(dataInputStream.read(bytes2)) < totalBytes) {
            if(ranges.containsKey(uuid)){
                long redData = ranges.get(uuid);
                redData =redData+red.longValue();
                ranges.replace(uuid,redData);
            }else {
                ranges.put(uuid, red.longValue());
            }
        }
        dataInputStream.close();

        InputStream input = new ByteArrayInputStream(bytes);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        artifactManagementService.validateAndStore(repositoryPath, input);

        String url = request.getRequestURI();
        response.reset();
        response.setDateHeader("Date",System.currentTimeMillis());
        response.setHeader("Docker-Distribution-Api-Version","registry/2.0");
        response.setHeader("Docker-Upload-UUID",uuid);
        response.setHeader("Location",url);
        response.setHeader("Range","0-"+bytes.length);
        response.setHeader("Content-Length","0");
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
            if(data.containsKey(digest)){

                response.addHeader("Docker-Distribution-Api-Version","registry/2.0");
                response.addHeader("Accept-Ranges","bytes");
                response.addHeader("Docker-Content-Digest",digest);
                return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
            }else {
                response.addHeader("Docker-Distribution-Api-Version","registry/2.0");
                response.addHeader("Accept-Ranges","bytes");
                response.addHeader("Docker-Content-Digest",digest);
                return new ResponseEntity<>("OK", HttpStatus.NOT_FOUND);
            }

        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }


    /**
     * Upload Progress 上传进度
     *
     * @param httpHeaders
     * @param request
     * @param response
     * @param digest
     */
    @RequestMapping(value = {"/v2/{repName}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public void uploadProgress(@RequestHeader HttpHeaders httpHeaders,
                               HttpServletRequest request,
                               HttpServletResponse response,
                               @PathVariable String repName,
                               @PathVariable String name,
                               @PathVariable Digest digest
    ) {
        //GET /v2/<name>/blobs/uploads/<uuid>
        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
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
            InputStream stream = new ByteArrayInputStream(json.toString().getBytes(StandardCharsets.UTF_8));
            String contentType = request.getContentType();
            //if(!Objects.equals("application/vnd.docker.distribution.manifest.v2+json",contentType)){

            // }else {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, stream);
            // }
            stream.close();
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
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

