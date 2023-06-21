package com.veadan.folib.controllers.layout.docker;


import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.lang.UUID;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.cloud.storage.s3fs.S3Iterator;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.domain.*;
import com.veadan.folib.enums.RepositoryScopeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.DirectoryListingService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.utils.DockerApiHeader;
import com.veadan.folib.utils.FileUtils;
import io.swagger.annotations.*;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.text.MessageFormat;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;


/**
 * REST API for all artifact-related processes.
 * <p>
 * Thanks to custom URL processing any path variable like '{artifactPath:.+}' will be processed as '**'.
 *
 * @author Martin Todorov
 * @author
 * @author veadan
 * @author @author veadan
 * @see{@linkplain http://docs.spring.io/spring/docs/current/spring-framework-reference/html/mvc.html#mvc-config-path-matching}
 */
@RestController
//@LayoutRequestMapping(DockerArtifactCoordinates.LAYOUT_NAME) docker工具访问接口路径从/v2开始，无法与/storages兼容
public class DockerArtifactController extends BaseArtifactController {

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;
    @Inject
    ArtifactRepository artifactRepository;


    /**
     * 文件进度
     */
    final ConcurrentHashMap<String, Long> ranges = new ConcurrentHashMap<String, Long>();

    /**
     * 镜像层 sha256 和分片 uuid
     */
    final ConcurrentHashMap<String, String> data = new ConcurrentHashMap<String, String>();


    /**
     * 检查终结点是否实现了 Docker 注册表 API V2。
     *
     * @param request
     * @param response
     * @return
     * @throws Exception
     */
    @ApiOperation(value = "v2")
    @PreAuthorize("authenticated")
    @RequestMapping(value = {"/v2/"}, method = {RequestMethod.GET})
    public ResponseEntity checkRepositoryAccess(@RequestHeader(value = HttpHeaders.AUTHORIZATION, required = false) String authorization,
                                                @RequestHeader HttpHeaders httpHeaders,
                                                HttpServletRequest request,
                                                HttpServletResponse response)
            throws Exception {
        response.reset();
        response.setDateHeader("Date", System.currentTimeMillis());
        response.setHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.addHeader("WWW-Authenticate", "BASIC realm=Folib Repository Manager");
        if (Objects.isNull(authorization)) {
            Map<String, Object> result = new HashMap<>(1);
            Map<String, Object> data = new HashMap<>(1);
            data.put("code", "UNAUTHORIZED");
            data.put("message", "access to the requested resource is not authorized");
            data.put("detail", null);
            List<Map> list = new ArrayList<>();
            list.add(data);
            result.put("errors", list);
            return new ResponseEntity<>(result, HttpStatus.UNAUTHORIZED);
        }
        return new ResponseEntity<>("ok", HttpStatus.OK);
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
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/"}, method = {RequestMethod.POST}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> startingAnUpload(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageId,
                                                   @PathVariable String repositoryId,
                                                   @PathVariable String name

    ) {
        final String path = name;
        try {
            String uuid = UUID.randomUUID().toString();
            String url = new StringBuffer().append(request.getRequestURI()).append(uuid).toString();

            response.reset();
            response.setDateHeader("Date", System.currentTimeMillis());
            response.setHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.setHeader(DockerApiHeader.DOCKER_UPLOAD_UUID.key(), uuid);
            response.setHeader(DockerApiHeader.LOCATION.key(), url);
            response.setHeader(DockerApiHeader.RANGE.key(), "bytes=0--1");
            response.setHeader(DockerApiHeader.CONTENT_LENGTH.key(), "0");

            return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }

    }


    @ApiOperation(value = "Upload Progress 上传进度")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.GET}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> uploadProgress(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String uuid

    ) {

        response.addHeader("Range", ranges.get(uuid).toString());
        return new ResponseEntity<>("OK", HttpStatus.NO_CONTENT);
    }

    @ApiOperation(value = "Monolithic Upload 单片上传")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.PUT}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> monolithicUpload(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageId,
                                                   @PathVariable String repositoryId,
                                                   @PathVariable String name,
                                                   @PathVariable String uuid,
                                                   @RequestParam String digest,
                                                   @RequestBody byte[] blobBytes

    ) {
        ResponseEntity result = new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
        try {
            int totalBytes = request.getContentLength();
            //totalBytes > 0
            if (totalBytes > 0) {
                FileUtils utils = new FileUtils();
                String fileDir = storageId + "/" + repositoryId + "/" + name;
                utils.upload(fileDir, uuid, 0, blobBytes);
                if (ranges.containsKey(uuid)) {
                    ranges.replace(uuid, ranges.get(uuid) + blobBytes.length);
                } else {
                    ranges.put(uuid, (long) blobBytes.length);
                }
            }
            totalBytes = ranges.get(uuid).intValue();
            if (data.containsKey(uuid)) {
                data.replace(digest, uuid);
            } else {
                data.put(digest, uuid);
            }
            InputStream inputStream = storageData(storageId, repositoryId, name, digest);
            String artifactPath = String.format("%s/blobs/%s", name, digest);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            logger.info("storageId:{},repositoryId:{},name:{},digest:{},uuid:{}", storageId, repositoryId, name, digest, uuid);
            artifactManagementService.validateAndStore(repositoryPath, inputStream);

            String url = new StringBuffer().append("http://").append(request.getRemoteHost()).append(request.getRequestURI()).toString();
            url = url.replace(uuid, digest);

            response.reset();
            response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
            response.setHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.setHeader(DockerApiHeader.LOCATION.key(), url);
            response.setHeader(DockerApiHeader.DOCKER_CONTENT_DIGEST.key(), digest);
            response.setHeader(DockerApiHeader.CONTENT_RANGE.key(), "0-" + totalBytes);
            //Content-Range	0-19778034
            //202 Accepted

        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            result = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        } finally {
            deleteLayers(storageId, repositoryId, name, digest);
            return result;
        }

    }


    @ApiOperation(value = "Chunked Upload 分片上传")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.PATCH}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> chunkedUpload(@RequestHeader HttpHeaders httpHeaders,
                                                HttpServletRequest request,
                                                HttpServletResponse response,
                                                @PathVariable String storageId,
                                                @PathVariable String repositoryId,
                                                @PathVariable String name,
                                                @PathVariable String uuid,
                                                @RequestBody byte[] inputStream


    ) throws Exception {
        response.setCharacterEncoding("utf8");
        byte[] bytes = inputStream;
        FileUtils utils = new FileUtils();
        String fileDir = new StringBuffer().append(storageId).append("/").append(repositoryId).append("/").append(name).toString();
        String fileName = uuid;
        utils.upload(fileDir, fileName, 0, bytes);

        if (ranges.containsKey(uuid)) {
            ranges.replace(uuid, ranges.get(uuid) + bytes.length);
        } else {
            ranges.put(uuid, (long) bytes.length);
        }
        String url = request.getRequestURI();
        response.reset();
        response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
        response.setHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.setHeader(DockerApiHeader.DOCKER_UPLOAD_UUID.key(), uuid);
        response.setHeader(DockerApiHeader.LOCATION.key(), url);
        response.setHeader(DockerApiHeader.RANGE.key(), "0-" + ranges.get(uuid).intValue());
        response.setHeader(DockerApiHeader.CONTENT_LENGTH.key(), "0");
        //202 Accepted
        return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
    }


    @ApiOperation(value = "Uploading the Layer 上传图层")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/uploads/{uuid}"}, method = {RequestMethod.POST}, consumes = MediaType.APPLICATION_OCTET_STREAM_VALUE)
    public ResponseEntity<String> uploadingTheLayer(@RequestHeader HttpHeaders httpHeaders,
                                                    HttpServletRequest request,
                                                    HttpServletResponse response,
                                                    @PathVariable String storageId,
                                                    @PathVariable String repositoryId,
                                                    @PathVariable String name

    ) {
        final String path = name;


        try {
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
    @ApiOperation(value = "Existing Layers 现有层")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "digest", value = "digest", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/{digest}"}, method = {RequestMethod.HEAD}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity existingLayers(@RequestHeader HttpHeaders httpHeaders,
                                         HttpServletRequest request,
                                         HttpServletResponse response,
                                         @PathVariable String storageId,
                                         @PathVariable String repositoryId,
                                         @PathVariable String name,
                                         @PathVariable String digest
    ) {
        try {
            //可以通过对 blob 存储 API 的请求来检查层是否存在。请求的格式应如下所示：HEAD
            response.reset();
            response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.addHeader("Accept-Ranges", "bytes");
            response.addHeader(DockerApiHeader.DOCKER_CONTENT_DIGEST.key(), digest);
            String artifactName = String.format("%s/blobs/%s", name, digest);
            boolean exist = artifactRepository.artifactExists(storageId, repositoryId, artifactName);
            //200已经存在 404不存在
            if (exist) {
                return new ResponseEntity<>("OK", HttpStatus.ACCEPTED);
            } else {
                return new ResponseEntity<>(null, HttpStatus.NOT_FOUND);
            }

        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }

    @ApiOperation(value = "Uploading the Layer 上传进度")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "uuid", value = "uuid", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repName}/blobs/uploads/{uuid}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity uploadingTheLayer(@RequestHeader HttpHeaders httpHeaders,
                                            HttpServletRequest request,
                                            HttpServletResponse response,
                                            @PathVariable String storageId,
                                            @PathVariable String repositoryId,
                                            @PathVariable String name,
                                            @PathVariable String uuid
    ) {
        FileUtils utils = new FileUtils();
        String fileDir = new StringBuffer().append(storageId).append("/").append(repositoryId).append("/").append(name).toString();
        String fileName = uuid;
        long range = utils.getOffset(fileDir, fileName);
        response.reset();
        response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
        response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.addHeader(DockerApiHeader.RANGE.key(), "bytes=0-" + range);
        response.addHeader(DockerApiHeader.LOCATION.key(), request.getRequestURI());
        response.addHeader(DockerApiHeader.DOCKER_UPLOAD_UUID.key(), uuid);
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
    @ApiOperation(value = "pushing An Image Manifest 推送镜像清单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "tag", value = "tag", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{tag}"}, method = {RequestMethod.PUT})
    public ResponseEntity pushingAnImageManifest(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String tag,
                                                 @RequestBody byte[] bytes
    ) {

        String manifestSha256 = null;
        ResponseEntity result = ResponseEntity.ok("The artifact was deployed successfully.");
        try {
            logger.info("manifest.json size:{}", bytes.length);
            manifestSha256 = imagesStorage(storageId, repositoryId, name, tag, bytes);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            result = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        } finally {
            response.reset();
            response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
            response.addHeader(DockerApiHeader.DOCKER_CONTENT_TYPE.key(), DockerApiHeader.DOCKER_CONTENT_TYPE.value());
            response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            response.addHeader(DockerApiHeader.DOCKER_CONTENT_DIGEST.key(), manifestSha256);
            return result;
        }
    }

    //镜像存储

    /**
     * 镜像文件零时存储
     *
     * @param storageId
     * @param repositoryId
     * @param name
     * @param tag
     * @return
     */
    String imagesStorage(String storageId,
                         String repositoryId,
                         String name,
                         String tag,
                         byte[] bytes) {
        InputStream stream = new ByteArrayInputStream(bytes);
        InputStream destStream = new ByteArrayInputStream(bytes);
        String manifestSha256 = null;
        try {
            //Use SHA-1 algorithm
            MessageDigest shaDigest = MessageDigest.getInstance("SHA-256");
            //SHA-1 checksum
            String shaChecksum = getFileChecksum(shaDigest, new ByteArrayInputStream(bytes));
            String sha256 = String.format("sha256:%s", shaChecksum);
            String artifactPath = String.format("%s/manifest/%s", name, sha256);


            //判断镜像清单是否在
            if (!mirrorLayerExists(artifactPath, storageId, repositoryId)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
                logger.info(String.valueOf(System.currentTimeMillis()));
                artifactManagementService.validateAndStore(repositoryPath, stream);
            }

            String destArtifactPath = String.format("%s/%s/%s", name, tag, sha256);
            String artifactName = String.format("%s/%s", name, tag);

            //判断镜像清单是否发生变化
            String tagSha256 = verifyTagSha256(artifactName, storageId, repositoryId);
            RepositoryPath destPath = repositoryPathResolver.resolve(storageId, repositoryId, destArtifactPath);

            //如果存在并发生变化删除更新
            if (Objects.isNull(tagSha256)) {
                artifactManagementService.validateAndStore(destPath, destStream);
            } else if (!Objects.equals(tagSha256, sha256)) {
                RepositoryPath deletePath = repositoryPathResolver.resolve(storageId, repositoryId, destArtifactPath.replace(sha256, tagSha256));
                artifactManagementService.delete(deletePath, true);
                artifactManagementService.validateAndStore(destPath, destStream);
            }

            //copy 没有存储数据库
            //artifactManagementService.copy(srcPath, destPath);
            manifestSha256 = sha256;
            destStream.close();
            stream.close();
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        } finally {

            return manifestSha256;
        }
    }


    private String getFileChecksum(MessageDigest digest, InputStream stream) throws IOException {
        //Get file input stream for reading the file content
        //FileInputStream fis = new FileInputStream(file);

        //Create byte array to read data in chunks
        byte[] byteArray = new byte[1024];
        int bytesCount = 0;

        //Read file data and update in message digest
        while ((bytesCount = stream.read(byteArray)) != -1) {
            digest.update(byteArray, 0, bytesCount);
        }
        ;

        //close the stream; We don't need it now.
        stream.close();

        //Get the hash's bytes
        byte[] bytes = digest.digest();

        //This bytes[] has bytes in decimal format;
        //Convert it to hexadecimal format
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < bytes.length; i++) {
            sb.append(Integer.toString((bytes[i] & 0xff) + 0x100, 16).substring(1));
        }

        //return complete hash
        return sb.toString();
    }

    /**
     * 删除临时目录文件
     *
     * @param storageId
     * @param repositoryId
     * @param name
     * @param digests
     */
    public void deleteLayers(String storageId,
                             String repositoryId,
                             String name,
                             String digests) {
        FileUtils utils = new FileUtils();

        if (data.containsKey(digests) && Objects.nonNull(data.get(digests))) {
            String fileDir = String.format("%s/%s/%s", storageId, repositoryId, name);
            utils.deleteDir(fileDir, data.get(digests));
            data.remove(digests);
        }

    }

    /**
     * 存储数据
     *
     * @param storageId
     * @param repositoryId
     * @param name
     * @param digest
     * @return
     */
    public InputStream storageData(String storageId, String repositoryId, String name, String digest) {
        FileUtils utils = new FileUtils();
        String fileDir = String.format("%s/%s/%s", storageId, repositoryId, name);
        String fileName = data.get(digest);
        return utils.getFile(fileDir, fileName);
    }

    ;


    @ApiOperation(value = "Existing Manifests 现有清单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "tag", value = "tag", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{tag}"}, method = {RequestMethod.HEAD}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity existingManifests(@RequestHeader HttpHeaders httpHeaders,
                                            HttpServletRequest request,
                                            HttpServletResponse response,
                                            @PathVariable String storageId,
                                            @PathVariable String repositoryId,
                                            @PathVariable String name,
                                            @PathVariable String tag) {

        String artifactName = String.format("%s/%s/", name, tag);
        //镜像不存在 404 Not Found
        ResponseEntity entity = ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        String manifest = getManifest(storageId, repositoryId, artifactName);
        if (StringUtils.isNotBlank(manifest)) {
            entity = ResponseEntity.status(HttpStatus.OK).build();
        }
        response.reset();
        response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
        response.addHeader(DockerApiHeader.DOCKER_CONTENT_TYPE.key(), DockerApiHeader.DOCKER_CONTENT_TYPE.value());
        response.addHeader(DockerApiHeader.RATELIMIT_LIMIT.key(), "100;w=21600");
        response.addHeader(DockerApiHeader.RATELIMIT_REMAINING.key(), "100;w=21600");
        response.addHeader(DockerApiHeader.STRICT_TRANSPORT_SECURITY.key(), "max-age=31536000");
        response.addHeader(DockerApiHeader.DOCKER_CONTENT_DIGEST.key(), manifest);
        response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
        response.addHeader(DockerApiHeader.DOCKER_RATELIMIT_SOURCE.key(), request.getRemoteHost());
        response.addHeader(DockerApiHeader.ETAG.key(), manifest);
        return entity;
    }

    public String verifyTagSha256(String artifactName, String storageId, String repositoryId) {
        try {
            return getLayers(artifactName, storageId, repositoryId);
        } catch (IOException e) {
            return null;
        }
    }


    //是否存在镜像层
    @NotNull
    private Boolean mirrorLayerExists(String artifactName, String storageId, String repositoryId) {

        return artifactRepository.artifactExists(storageId, repositoryId, artifactName);
    }

    private String getLayers(String artifactName, String storageId, String repositoryId) throws IOException {
        Artifact artifacts = getArtifact(artifactName, storageId, repositoryId);
        String layers = null;
        if (Objects.nonNull(artifacts)) {
            Map<String, String> mapCoordinates = artifacts.getArtifactCoordinates().getCoordinates();

            if (Objects.nonNull(mapCoordinates) && mapCoordinates.containsKey("layers")) {
                layers = mapCoordinates.get("layers");
            }
        }
        return layers;
    }

    public Artifact getArtifact(String artifactName, String storageId, String repositoryId) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactName);
        Path path = repositoryPath.getTarget();
        String artifactPath = "";
        if (path instanceof S3Path) {
            //S3存储
            S3Path s3Path = (S3Path) path;
            S3Iterator iterators = new S3Iterator(s3Path);
            S3Path imagePath = null;
            while (iterators.hasNext()) {
                S3Path itemS3Path = iterators.next();
                if (!itemS3Path.endsWith(".sha256")) {
                    imagePath = itemS3Path;
                    break;
                }
            }
            if (Objects.nonNull(imagePath)) {
                artifactPath = imagePath.getKey().replace(String.format("%s/%s/", repositoryPath.getStorageId(), repositoryPath.getRepositoryId()), "");
            }
        } else {
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
            FileContent fileContent = fileContents.get(0);
            artifactPath = fileContent.getArtifactPath();
        }
        return artifactRepository.findOneArtifact(storageId, repositoryId, artifactPath);
    }


    @ApiOperation(value = "Pulling an Image Manifest 获取镜像清单")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "digest", value = "digest", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/manifests/{digest}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity pullingAnImageManifest(@RequestHeader HttpHeaders httpHeaders,
                                                 HttpServletRequest request,
                                                 HttpServletResponse response,
                                                 @PathVariable String storageId,
                                                 @PathVariable String repositoryId,
                                                 @PathVariable String name,
                                                 @PathVariable String digest) {

        ResponseEntity entity = ResponseEntity.status(HttpStatus.NOT_FOUND).build();
        if (!digest.startsWith("sha256:")) {
            String artifactName = String.format("%s/%s/", name, digest);
            String manifest = getManifest(storageId, repositoryId, artifactName);
            if (StringUtils.isBlank(manifest)) {
                return entity;
            }
            digest = manifest;
        }
        try {
            String artifactPath = String.format("%s/manifest/%s", name, digest);
            logger.info("pullingAnImageManifest params [storageId:{}, repositoryId:{}, artifactPath:{}", storageId, repositoryId, artifactPath);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            response.reset();
            response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
            entity = ResponseEntity.status(HttpStatus.OK).build();
            response.addHeader(DockerApiHeader.DOCKER_CONTENT_DIGEST.key(), digest);
            response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        return entity;
    }

    @ApiOperation(value = "Pulling a Layer 拉取镜像层")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "制品名", required = true),
            @ApiImplicitParam(name = "digest", value = "digest", required = true)
    })
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/blobs/{digest}"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity pullingALayer(@RequestHeader HttpHeaders httpHeaders,
                                        HttpServletRequest request,
                                        HttpServletResponse response,
                                        @PathVariable String storageId,
                                        @PathVariable String repositoryId,
                                        @PathVariable String name,
                                        @PathVariable String digest) {

        String artifactPath = String.format("%s/blobs/%s", name, digest);
        ResponseEntity entity = ResponseEntity.status(HttpStatus.NOT_FOUND).build();


        try {
            logger.info("pullingALayer params [storageId:{}, repositoryId:{}, artifactPath:{}", storageId, repositoryId, artifactPath);
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
            vulnerabilityBlock(repositoryPath);
            response.reset();
            response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());

            response.addHeader(DockerApiHeader.DOCKER_CONTENT_DIGEST.key(), digest);
            response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
            entity = ResponseEntity.status(HttpStatus.OK).build();
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        }
        return entity;
    }

    @ApiOperation(value = "Listing Image Tags 获取镜像tag列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "storageId", value = "存储id", required = true),
            @ApiImplicitParam(name = "repositoryId", value = "仓库id", required = true),
            @ApiImplicitParam(name = "name", value = "镜像名称", required = true),
            @ApiImplicitParam(name = "n", value = "返回个数"),
            @ApiImplicitParam(name = "last", value = "last")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/{storageId}/{repositoryId}/{name}/tags/list"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity<Object> listingImageTags(@RequestHeader HttpHeaders httpHeaders,
                                                   HttpServletRequest request,
                                                   HttpServletResponse response,
                                                   @PathVariable String storageId,
                                                   @PathVariable String repositoryId,
                                                   @PathVariable String name,
                                                   @RequestParam(name = "n", required = false) Integer n,
                                                   @RequestParam(name = "last", required = false) String last) {
        try {
            logger.info("Listing Image Tags [storageId:{}, repositoryId:{}, name:{}, n:{}, last:{}]", storageId, repositoryId, name, n, last);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, name);
            List<FileContent> imageDirList = null;
            if (Files.exists(repositoryPath)) {
                DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
                imageDirList = directoryListing.getDirectories().stream().filter(f -> (!f.getName().equals("blobs")) && (!f.getName().equals("manifest"))).collect(Collectors.toList());
            }
            List<String> tagList = Optional.ofNullable(imageDirList).orElse(Collections.emptyList()).stream().map(FileContent::getName).collect(Collectors.toList());
            List<String> resultList;
            int size = tagList.size(), startIndex = 0, endIndex = size;
            if (StringUtils.isNotBlank(last)) {
                int index = tagList.indexOf(last);
                if (index != -1) {
                    startIndex = index + 1;
                }
            }
            if (startIndex > size) {
                startIndex = size;
            }
            if (Objects.nonNull(n)) {
                if (n < 1) {
                    n = size;
                }
                endIndex = startIndex + n;
            }
            if (endIndex > size) {
                endIndex = size;
            }
            String link = "";
            resultList = tagList.subList(startIndex, endIndex);
            if (CollectionUtils.isNotEmpty(resultList)) {
                last = resultList.get(resultList.size() - 1);
                if (Objects.nonNull(n) && n > 0 & endIndex <= size - 1) {
                    link = "</v2/{0}/tags/list?last={1}&n={2}>; rel=\"next\"";
                    link = MessageFormat.format(link, name, last, n);
                }
            }
            logger.info("Listing Image Tags [storageId:{}, repositoryId:{}, name:{}, startIndex:{}, endIndex:{}, link:{}]", storageId, repositoryId, name, startIndex, endIndex, link);
            DockerTags dockerTags = DockerTags.builder().name(name).tags(resultList).build();
            response.reset();
            response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
            response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
            if (StringUtils.isNotBlank(link)) {
                response.addHeader(HttpHeaders.LINK, link);
            }
            return ResponseEntity.ok(dockerTags);
        } catch (Exception ex) {
            logger.error("Listing Image Tags [storageId:{}, repositoryId:{}, name:{}, n:{}, last:{} error {}]", storageId, repositoryId, name, n, last, ExceptionUtils.getStackTrace(ex));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ex.getMessage());
        }
    }

    @ApiOperation(value = "Get Catalog 获取仓库列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "n", value = "返回个数"),
            @ApiImplicitParam(name = "last", value = "last")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @RequestMapping(value = {"/v2/_catalog"}, method = {RequestMethod.GET}, consumes = MediaType.ALL_VALUE)
    public ResponseEntity<Object> getCatalog(@RequestHeader HttpHeaders httpHeaders,
                                             HttpServletRequest request,
                                             HttpServletResponse response,
                                             @RequestParam(name = "n", required = false) Integer n,
                                             @RequestParam(name = "last", required = false) String last,
                                             Authentication authentication) {
        try {
            logger.info("GET Catalog [n:{}, last:{}]", n, last);
            List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration()
                    .getStorages()
                    .values());
            String username = "";
            if (Objects.nonNull(authentication)) {
                final UserDetails loggedUser = (UserDetails) authentication.getPrincipal();
                username = loggedUser.getUsername();
            }
            String link = "", next = "";
            List<String> resultList = Collections.emptyList();
            if (CollectionUtil.isNotEmpty(storageList)) {
                boolean filterByUser = !hasAdmin();
                String finalUsername = username;
                storageList = storageList.stream()
                        .distinct()
                        .filter(s -> !filterByUser || (CollectionUtil.isNotEmpty(s.getUsers()) && s.getUsers().contains(finalUsername)) ||
                                (CollectionUtils.isNotEmpty(s.getRepositories().values()) && s.getRepositories().values().stream().anyMatch(repository -> RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()))))
                        .collect(Collectors.toCollection(LinkedList::new));
                List<Repository> repositories;
                List<String> repositoryList = Lists.newArrayList();
                for (Storage storage : storageList) {
                    boolean flag = !hasAdmin() && !username.equals(storage.getAdmin()) && (CollectionUtils.isNotEmpty(storage.getUsers()) && !storage.getUsers().contains(username));
                    repositories = new LinkedList<Repository>(storage.getRepositories().values());
                    repositories = repositories.stream().distinct()
                            .filter(r -> DockerLayoutProvider.ALIAS.equalsIgnoreCase(r.getLayout()))
                            .collect(Collectors.toCollection(LinkedList::new));
                    if (flag) {
                        repositories = repositories.stream().filter((item -> RepositoryScopeEnum.OPEN.getType().equals(item.getScope()))).collect(Collectors.toList());
                    }
                    if (CollectionUtils.isNotEmpty(repositories)) {
                        repositoryList.addAll(repositories.stream().map(item -> String.format("%s/%s", item.getStorage().getId(), item.getId())).collect(Collectors.toList()));
                    }
                }
                int size = repositoryList.size(), startIndex = 0, endIndex = size;
                if (StringUtils.isNotBlank(last)) {
                    int index = repositoryList.indexOf(last);
                    if (index != -1) {
                        startIndex = index + 1;
                    }
                }
                if (startIndex > size) {
                    startIndex = size;
                }
                if (Objects.nonNull(n)) {
                    if (n < 1) {
                        n = size;
                    }
                    endIndex = startIndex + n;
                }
                if (endIndex > size) {
                    endIndex = size;
                }
                resultList = repositoryList.subList(startIndex, endIndex);
                if (CollectionUtils.isNotEmpty(resultList)) {
                    last = resultList.get(resultList.size() - 1);
                    if (Objects.nonNull(n) && n > 0 & endIndex <= size - 1) {
                        link = "</v2/_catalog?last={0}&n={1}>; rel=\"next\"";
                        link = MessageFormat.format(link, last, n);

                        next = "/v2/_catalog?last={0}&n={1}";
                        next = MessageFormat.format(next, last, n);
                    }
                }
                logger.info("GET Catalog [n:{}, last:{} startIndex:{}, endIndex:{}, link:{}]", n, last, startIndex, endIndex, link);
            }
            DockerCatalog dockerCatalog = DockerCatalog.builder().next(next).repositories(resultList).build();
            response.reset();
            response.setDateHeader(DockerApiHeader.DATE.key(), System.currentTimeMillis());
            response.addHeader(DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.key(), DockerApiHeader.DOCKER_DISTRIBUTION_API_VERSION.value());
//            response.addHeader(DockerApiHeader.CONTENT_LENGTH.key(), dockerCatalog);
            if (StringUtils.isNotBlank(link)) {
                response.addHeader(HttpHeaders.LINK, link);
            }
            return ResponseEntity.ok(dockerCatalog);
        } catch (Exception ex) {
            logger.error("GET Catalog [n:{}, last:{} error {}]", n, last, ExceptionUtils.getStackTrace(ex));
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(ex.getMessage());
        }
    }

    /**
     * 获取manifest
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactName 制品名称
     * @return manifest
     */
    private String getManifest(String storageId, String repositoryId, String artifactName) {
        Artifact artifact = null;
        String manifest = "";
        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactName);
            DirectoryListing directoryListing = directoryListingService.fromRepositoryPath(repositoryPath);
            List<FileContent> fileContents = directoryListing.getFiles().stream().filter(file -> !(file.getName().endsWith(".sha256"))).collect(Collectors.toList());
            FileContent fileContent = fileContents.get(0);
            artifact = artifactRepository.findOneArtifact(storageId, repositoryId, fileContent.getArtifactPath());
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
        } finally {
            if (Objects.nonNull(artifact)) {
                Map<String, String> mapCoordinates = artifact.getArtifactCoordinates().getCoordinates();
                if (Objects.nonNull(mapCoordinates) && mapCoordinates.containsKey("layers")) {
                    manifest = mapCoordinates.get("layers");
                }
            }
        }
        return manifest;
    }

}

