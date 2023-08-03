package com.veadan.folib.controllers.layout.cocoapods;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.util.StrUtil;
import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.controllers.BaseArtifactController;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.CocoapodsArtifactUtil;
import com.veadan.folib.web.LayoutRequestMapping;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/3 15:29
 * @since x.x.x
 */
@RestController
@LayoutRequestMapping(CocoapodsArtifactCoordinates.LAYOUT_NAME)
public class CocoapodsArtifactController extends BaseArtifactController
{
    @ApiOperation(value = "Used to deploy an artifact")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The artifact was deployed successfully."),
            @ApiResponse(code = 400, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('ARTIFACTS_DEPLOY')")
    @PutMapping(value = "{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity uploadPod(@RepositoryMapping Repository repository,
                                 @PathVariable String artifactPath,
                                 HttpServletRequest request) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        try {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
            artifactManagementService.validateAndStore(repositoryPath, request.getInputStream());

            // 读取pod.tar.gz中的*.podspec文件内容
            final Path path = repositoryPath.getTarget();
            final BufferedInputStream podTarGzInputStream = FileUtil.getInputStream(path);
            final String podspecSourceContent = CocoapodsArtifactUtil.fetchPodspecSourceContent(podTarGzInputStream);
            if (StringUtils.isNotBlank(podspecSourceContent))
            {
                final CocoapodsArtifactUtil.PodSpec podSpec = CocoapodsArtifactUtil.resolvePodSpec(podspecSourceContent);
                final ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(podspecSourceContent.getBytes(StandardCharsets.UTF_8));
                final RepositoryPath podSpecRepositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, String.format(".specs/%s/%s.podspec", podSpec.getVersion(), podSpec.getName()));
                artifactManagementService.store(podSpecRepositoryPath, byteArrayInputStream);
            }

            return ResponseEntity.ok("The artifact was deployed successfully.");
        } catch (Exception e) {
            logger.error(e.getMessage(), e);

            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
        }
    }
}
