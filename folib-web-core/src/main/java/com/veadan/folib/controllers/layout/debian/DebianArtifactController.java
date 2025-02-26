package com.veadan.folib.controllers.layout.debian;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author huayanjun
 * @since 2024-09-06 10:36
 */
@RestController
@LayoutRequestMapping(DebianConstant.LAYOUT_NAME)
@Api(tags = "debian 坐标控制器")
public class DebianArtifactController extends BaseArtifactController {

    @Override
    @GetMapping(value = {"/{repositoryId}/"})
    public ResponseEntity<String> checkRepositoryAccess() {
        return super.checkRepositoryAccess();
    }

    @ApiOperation(value = "Used to retrieve an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = ""),
            @ApiResponse(code = 404, message = "Requested path not found."),
            @ApiResponse(code = 500, message = "Server error."),
            @ApiResponse(code = 503, message = "Repository currently not in service.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_VIEW')")
    @AuditLog(value = AuditEventNameEnum.DOWNLOAD_EXCEPTION, target = "#storageId + '/' + #repositoryId + '/' + #artifactPath")
    @RequestMapping(value = {"/{repositoryId}/{artifactPath:.+}"}, method = {RequestMethod.GET, RequestMethod.HEAD})
    public Object download(
            @RepositoryMapping Repository repository,
            @RequestHeader HttpHeaders httpHeaders,
            @PathVariable String artifactPath,
            @PathVariable String repositoryId,
            HttpServletRequest request,
            HttpServletResponse response,
            ModelMap model)
            throws Exception {
        return super.download(repository, httpHeaders, artifactPath, request, response, model);
    }

}
