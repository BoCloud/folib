package com.veadan.folib.controllers.layout.docker;


import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.controllers.BaseArtifactController;

import com.veadan.folib.providers.io.RepositoryPath;

import io.swagger.annotations.ApiOperation;

import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.codec.cli.Digest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;


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
//            if(!Objects.equals("application/vnd.docker.distribution.manifest.v2+json",contentType)){
//                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
//                artifactManagementService.validateAndStore(repositoryPath, request.getInputStream());
//            }

            return ResponseEntity.ok("The artifact was deployed successfully.");
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
        final String artifactPath = name +"/"+ digest;

        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, request.getInputStream());
            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }


    /**
     * Uploading the Layer 上传图层
     *
     * @param httpHeaders
     * @param request
     * @param response
     */
    @RequestMapping(value = {"/v2/{storageName}/{repName}/{name}/blobs/{uuid}"}, method = {RequestMethod.POST}, consumes = MediaType.ALL_VALUE)
    public void uploadingTheLayer(@RequestHeader HttpHeaders httpHeaders,
                                  HttpServletRequest request,
                                  HttpServletResponse response,
                                  @PathVariable String storageName,
                                  @PathVariable String repName,
                                  @PathVariable String name,
                                  @PathVariable String uuid
    ) {
        ///v2/<name>/blobs/uploads/<uuid>
        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
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

        final String artifactPath = name +"/"+ tag+"/manifest.json";

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

