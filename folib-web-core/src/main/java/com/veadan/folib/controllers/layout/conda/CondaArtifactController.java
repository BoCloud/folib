package com.veadan.folib.controllers.layout.conda;

import cn.hutool.http.server.HttpServerResponse;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.controllers.login.LoginController;
import com.veadan.folib.services.impl.BackupServiceImpl;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.UserUtils;
import com.veadan.folib.web.LayoutRequestMapping;
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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import javax.json.JsonObject;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DELETE;

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

    @ApiOperation(value = "")
    @ApiResponses(value = {@ApiResponse(code = 404, message = "Package Not Found")})
    @GetMapping(path = "/{storageId}/{repositoryId}/dist/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity<Object> getArtifact() {

        return null;
    }

    @GetMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}/{version}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity<Object> getPackage() {
        return null;
    }

    @GetMapping(path = "/{storageId}/{repositoryId}/release/{channelId}/{packageId}/{version}")
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    public ResponseEntity<Object> getRelease() {
        return null;
    }

    @PostMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}/{version}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity<Object> createPackage() {
        return null;
    }

    @PostMapping(path = "/{storageId}/{repositoryId}/release/{channelId}/{packageId}/{version}")
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    public ResponseEntity<Object> createRelease() {
        return null;
    }

    @ApiOperation(value = "Post the metadata of the artifact")
    @PostMapping(path = "/{storageId}/{repositoryId}/stage/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    public ResponseEntity<Object> stageArtifact(JsonObject metadata,
                                                HttpServletRequest request,
                                                HttpServerResponse response,
                                                Authentication authentication) {
        return null;
    }

    @PostMapping(path = "/{storageId}/{repositoryId}/commit/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    public ResponseEntity<Object> commitArtifact() {
        return null;
    }

    @DeleteMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}")
    public ResponseEntity<Object> deletePackage() {
        return null;
    }

    Boolean checkCondaPackageName(String packageName) {
        return true;
    }
}
