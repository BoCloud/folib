package com.veadan.folib.controllers.layout.docker;

import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.controllers.layout.maven.MavenRepositoryIndexPathTransformer;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.ArtifactStorageException;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.Map;

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
 * @see {@linkplain http://docs.spring.io/spring/docs/current/spring-framework-reference/html/mvc.html#mvc-config-path-matching}
 */
@RestController
public class DockerArtifactController
        extends BaseArtifactController {

    //@PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @RequestMapping(value = {"/v2/"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public ResponseEntity<String> checkRepositoryAccess(@RequestHeader HttpHeaders httpHeaders,
                                                        HttpServletRequest request,
                                                        HttpServletResponse response, @RequestBody(required = false) Object body)
            throws Exception {
        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
        return super.checkRepositoryAccess();
    }

    //@PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @RequestMapping(value = {"/auth/"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public void checkRepositoryAccess(@RequestHeader HttpHeaders httpHeaders,
                                      HttpServletRequest request,
                                      HttpServletResponse response,
                                      @RequestParam String account,
                                      @RequestParam String client_id,
                                      @RequestParam String offline_token,
                                      @RequestParam String service
    ) {
        Map<String,String> data = new HashMap<>();
        data.put("access_token","11111111111111");
        data.put("token","111111111111111");


        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
        new ResponseEntity<>(data, HttpStatus.OK);
    }


    //GET /v2/<name>/manifests/<reference>
//    @PreAuthorize("authenticated")
//    @RequestMapping(value = {"//v2/"}, method = {RequestMethod.GET, RequestMethod.HEAD})
//    public void pullingAnImage(@RequestHeader HttpHeaders httpHeaders,
//                                      HttpServletRequest request,
//                                      HttpServletResponse response,
//                                      @RequestParam String account,
//                                      @RequestParam String client_id,
//                                      @RequestParam String offline_token,
//                                      @RequestParam String service
//    ) {
//        Map<String,String> data = new HashMap<>();
//        data.put("access_token","11111111111111");
//        data.put("token","111111111111111");
//
//
//        System.out.printf(request.toString());
//        System.out.printf(httpHeaders.toString());
//        new ResponseEntity<>(data, HttpStatus.OK);
//    }

    //https://registry.cn-hangzhou.aliyuncs.com/v2/kuaidaoqingyi-public/node/manifests/v9

   // PUT /v2/<name>/manifests/<reference>
    //Pushing an Image Manifest
   @RequestMapping(value = {"/v2/{name}/manifests/{tag}/"}, method = {RequestMethod.PUT})
   public void pushingAnImageManifest(@RequestHeader HttpHeaders httpHeaders,
                           HttpServletRequest request,
                           HttpServletResponse response,
                           @PathVariable String repName ,@PathVariable String name,@PathVariable String tag
   ) {
        Map<String,String> data = new HashMap<>();
        data.put("access_token","11111111111111");
        data.put("token","111111111111111");


        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
        new ResponseEntity<>(data, HttpStatus.OK);
   }

    //POST /v2/<name>/blobs/uploads/

    @RequestMapping(value = {"/v2/{repName}/{name}/blobs/uploads"}, method = {RequestMethod.POST} ,consumes = MediaType.ALL_VALUE)
    public void pushAnImage(@RequestHeader HttpHeaders httpHeaders,
                               HttpServletRequest request,
                               HttpServletResponse response //,
//                               @PathVariable String repName ,
//                            @PathVariable String name,
//                            @PathVariable String tag,
//                            @RequestParam("file") MultipartFile file
    ) {
        //String fileName = file.getOriginalFilename();
        Map<String,String> data = new HashMap<>();
        data.put("access_token","11111111111111");
        data.put("token","111111111111111");


        System.out.printf(request.toString());
        System.out.printf(httpHeaders.toString());
        new ResponseEntity<>(data, HttpStatus.OK);
    }
}
