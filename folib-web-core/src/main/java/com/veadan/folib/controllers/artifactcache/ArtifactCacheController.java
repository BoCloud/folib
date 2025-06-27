package com.veadan.folib.controllers.artifactcache;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.services.ArtifactCacheRecordService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.math.BigDecimal;

/**
 * @author veadan
 */
@Slf4j
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/artifactCache")
@Api(description = "制品缓存管理", tags = "制品缓存管理")
public class ArtifactCacheController extends BaseController {

    @Inject
    private ArtifactCacheRecordService artifactCacheRecordService;

    @ApiOperation(value = "清空缓存目录")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping(value = "/cleanupArtifactCacheDirectory")
    public ResponseEntity<Object> cleanupArtifactCacheDirectory(@RequestParam String directoryPath) throws Exception {
        artifactCacheRecordService.cleanupArtifactCacheDirectory(directoryPath);
        return ResponseEntity.ok("");
    }

    @ApiOperation(value = "获取缓存目录已使用大小")
    @AuditLog(value = AuditEventNameEnum.CACHE_STRATEGY,target ="#directoryPath" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping(value = "/artifactCacheDirectoryUseSize")
    public ResponseEntity<BigDecimal> artifactCacheDirectoryUseSize(@RequestParam String directoryPath, @RequestParam(required = false) String unit) throws Exception {
        return ResponseEntity.ok(artifactCacheRecordService.artifactCacheDirectoryUseSize(directoryPath, unit));
    }
}
