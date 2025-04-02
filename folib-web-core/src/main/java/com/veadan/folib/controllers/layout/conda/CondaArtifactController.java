package com.veadan.folib.controllers.layout.conda;

import com.veadan.folib.artifact.coordinates.CondaArtifactCoordinates;
import com.veadan.folib.web.LayoutRequestMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.ws.rs.DELETE;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
@RestController
@LayoutRequestMapping(CondaArtifactCoordinates.LAYOUT_NAME)
@Slf4j
@Api(description = "conda坐标控制器",tags = "conda坐标控制器")
public class CondaArtifactController {

    @ApiOperation(value = "Used to authenticate an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "{token, expires_at}")})
    @PostMapping(path = "/{storageId}/{repositoryId}/authentications")
    public ResponseEntity<Object> authenticate() {
        return null;
    }

    @GetMapping(path = "/{storageId}/{repositoryId}/user")
    public ResponseEntity<Object> getUser() {
        return null;
    }

    @GetMapping(path = "/{storageId}/{repositoryId}/dist/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    public ResponseEntity<Object> getArtifact() {
        return null;
    }

    @GetMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}/{version}")
    public ResponseEntity<Object> getPackage() {
        return null;
    }

    @GetMapping(path = "/{storageId}/{repositoryId}/release/{channelId}/{packageId}/{version}")
    public ResponseEntity<Object> getRelease() {
        return null;
    }

    @PostMapping(path = "/{storageId}/{repositoryId}/package/{channelId}/{packageId}/{version}")
    public ResponseEntity<Object> createPackage() {
        return null;
    }

    @PostMapping(path = "/{storageId}/{repositoryId}/release/{channelId}/{packageId}/{version}")
    public ResponseEntity<Object> createRelease() {
        return null;
    }

    @PostMapping(path = "/{storageId}/{repositoryId}/stage/{channelId}/{packageId}/{version}/{platformId}/{filename}")
    public ResponseEntity<Object> stageArtifact() {
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
}
