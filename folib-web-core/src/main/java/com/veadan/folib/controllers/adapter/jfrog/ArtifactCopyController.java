package com.veadan.folib.controllers.adapter.jfrog;

import com.veadan.folib.controllers.BaseController;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author leipenghui
 */
@Slf4j
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog拷贝", tags = "JFrog拷贝")
public class ArtifactCopyController extends BaseController {

    @ApiOperation(value = "JFrog拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @RequestMapping("/api/copy")
    public ResponseEntity<Object> copy() {
        return ResponseEntity.ok("");
    }

    @ApiOperation(value = "JFrog镜像拷贝")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @RequestMapping("/api/docker")
    public ResponseEntity<Object> dockerCopy() {
        return ResponseEntity.ok("");
    }
}
