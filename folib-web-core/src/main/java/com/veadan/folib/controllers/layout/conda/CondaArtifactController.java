package com.veadan.folib.controllers.layout.conda;

import cn.hutool.http.server.HttpServerResponse;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.controllers.login.LoginController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.UserUtils;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;

import javax.json.JsonObject;
import javax.ws.rs.HEAD;
import javax.ws.rs.core.MediaType;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;
import java.nio.file.Files;
import java.util.Locale;


/**
 * @author LingengMa
 * @date 2025/04/07 09:12
 */
@RestController
@LayoutRequestMapping(CondaArtifactCoordinates.LAYOUT_NAME)
@Slf4j
@Api(description = "conda坐标控制器",tags = "conda坐标控制器")
public class CondaArtifactController extends BaseArtifactController {

    @Autowired
    private LoginController loginController;

    @Autowired
    RepositoryPathResolver repositoryPathResolver;


    @ApiOperation(value = "Check access to the repository")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK"),
                           @ApiResponse(code = 404, message = "Repository Not Found")})
    @RequestMapping(path = "/{storageId}/{repositoryId}", method = RequestMethod.HEAD)
    public ResponseEntity checkAccess(@RepositoryMapping Repository repository) {
        if (repository == null) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body("Repository Not Found");
        }
        return ResponseEntity.ok().build();
    }

    @ApiOperation(value = "Get Authentication Type")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(path = "/{storageId}/{repositoryId}/authentication-type")
    public ResponseEntity getAuthenticationType(@RepositoryMapping Repository repository) {
        JSONObject resultData = new JSONObject();
        resultData.put("authentication_type", "password");
        return ResponseEntity.ok(resultData);
    }

    @ApiOperation(value = "Used to authenticate an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "{token}")})
    @PostMapping(path = "/{storageId}/{repositoryId}/authentications")
    public ResponseEntity authenticate(Authentication authentication) {
         return loginController.formLogin(authentication);
    }


    @ApiOperation(value = "Used to get the user information")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "{login, user_type}"),
                           @ApiResponse(code = 401, message = "Unauthorized"),
                           @ApiResponse(code = 500, message = "Internal Server Error")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @GetMapping(path = "/{storageId}/{repositoryId}/user")
    public ResponseEntity<Object> getUser(HttpServletRequest request,
                                          HttpServerResponse response,
                                          Authentication authentication) {
        try {
            SpringSecurityUser currentUser = UserUtils.getSpringSecurityUser();
            if (currentUser == null) {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Unauthorized");
            }
            JSONObject responseJson = new JSONObject();
            responseJson.put("login", currentUser.getUsername());
            responseJson.put("user_type", currentUser.getUserType());
            return ResponseEntity.ok(responseJson);


        } catch (Exception e) {
            log.error("Error getting user information", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }


    @ApiOperation(value = "Get the metadata of the artifact")
    @ApiResponses(value = {@ApiResponse(code = 404, message = "Artifact Not Found")})
    @GetMapping(path = "/{storageId}/{repositoryId}/dist/{channelId}/{packageId}/{version}/{platformId}/{filename}",
                produces = MediaType.APPLICATION_JSON)
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity getArtifactMetadata(@RepositoryMapping Repository repository,
                                                      @PathVariable(name = "storageId") String storageId,
                                                      @PathVariable(name = "repositoryId") String repositoryId,
                                                      @PathVariable(name = "channelId") String channelId,
                                                      @PathVariable(name = "platformId") String platformId,
                                                      @PathVariable(name = "filename") String filename){
        String path = String.format("%s/%s/%s/index.json", channelId, platformId, filename);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);

        return readFileAsResponse(repositoryPath, "Artifact Not Found");
    }


    @ApiOperation(value = "Get the metadata of the artifacts in the package")
    @ApiResponses(value = {@ApiResponse(code = 404, message = "Package Not Found")})
    @GetMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}",
                produces = MediaType.APPLICATION_JSON)
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity getPackageMetadata(@RepositoryMapping Repository repository) {
        JSONObject resultData = new JSONObject();
        resultData.put("result", "success");
        return ResponseEntity.ok(resultData);
    }


    @ApiOperation(value = "Get the metadata of the artifacts in the release")
    @ApiResponses(value = {@ApiResponse(code = 404, message = "Release Not Found"),
                           @ApiResponse(code = 500, message = "Internal Server Error")})
    @GetMapping(path = "/{storageId}/{repositoryId}/release/{releasePath:.+}",
                produces = MediaType.APPLICATION_JSON)
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity getReleaseMetadata(@RepositoryMapping Repository repository) {
        JSONObject resultData = new JSONObject();
        resultData.put("result", "success");
        return ResponseEntity.ok(resultData);
    }


    @ApiOperation(value = "Post the metadata of the artifact to the package")
    @PostMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}/{version}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity createPackage() {
        JSONObject resultData = new JSONObject();
        resultData.put("result", "success");
        return ResponseEntity.ok(resultData);
    }


    @ApiOperation(value = "Post the metadata of the artifact to the release")
    @PostMapping(path = "/{storageId}/{repositoryId}/release/{channelId}/{packageId}/{version}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity createRelease() {
        JSONObject resultData = new JSONObject();
        resultData.put("result", "success");
        return ResponseEntity.ok(resultData);
    }


    @ApiOperation(value = "Post the metadata of the artifact")
    @PostMapping(path = "/{storageId}/{repositoryId}/stage/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    public ResponseEntity<Object> stageArtifact(JsonObject metadata,
                                                HttpServletRequest request,
                                                HttpServerResponse response,
                                                Authentication authentication) {
        return null;
    }


    @ApiOperation(value = "Commit the artifact")
    @PostMapping(path = "/{storageId}/{repositoryId}/commit/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    public ResponseEntity<Object> commitArtifact() {
        return null;
    }


    @DeleteMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}")
    public ResponseEntity<Object> deletePackage() {
        return null;
    }

    private ResponseEntity<Object> readFileAsResponse(RepositoryPath repositoryPath, String notFoundMessage) {
        if (!Files.exists(repositoryPath)) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND)
                    .body(notFoundMessage);
        }

        try {
            String content = new String(Files.readAllBytes(repositoryPath));
            return ResponseEntity.ok(content);
        } catch (Exception e) {
            log.error("Error reading file at path: {}", repositoryPath, e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }
}
