package com.veadan.folib.controllers.layout.go;


import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
@RestController
@LayoutRequestMapping(GoLayoutProvider.ALIAS)
@Slf4j
@Api(description = "go坐标控制器",tags = "go坐标控制器")
public class GoArtifactController extends BaseArtifactController {

    @Override
    @PreAuthorize("authenticated")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

//
//    @ApiOperation(value = "Used to retrieve an sumdb(not support)")
//    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
//    @ApiResponse(code = 400, message = "An error occurred.")})
//    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"sumdb/{url:.+}"})
    public  ResponseEntity<Object> sumdb(@PathVariable String url)
            throws Exception {
        //校验和数据库服务 暂不支持
        return ResponseEntity.status(HttpStatus.FORBIDDEN).body("not support sumdb");
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
    @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_RESOLVE')")
    @GetMapping(value = {"{storageId}/{repositoryId}/{artifactPath:.+}"})
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#repository.getStorage().getId() + '/' + #repository.getId() + '/' + #artifactPath")
    public ResponseEntity<Object> download(@RepositoryMapping Repository repository,
                                           @RequestHeader HttpHeaders httpHeaders,
                                           @PathVariable String artifactPath,
                                           HttpServletRequest request,
                                           HttpServletResponse response)
            throws Exception {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        logger.info("Requested /{}/{}/{}.", storageId, repositoryId, artifactPath);

        RepositoryPath repositoryPath = artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
        vulnerabilityBlock(repositoryPath);
        provideArtifactDownloadResponse(request, response, httpHeaders, repositoryPath);
        return null;
    }
}