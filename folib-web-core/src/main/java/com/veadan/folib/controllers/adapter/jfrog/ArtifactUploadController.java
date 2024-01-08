package com.veadan.folib.controllers.adapter.jfrog;

import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.dto.ArtifactUploadAdapterJfrogDto;
import com.veadan.folib.dto.ArtifactUploadAdapterJfrogDto.Checksums;
import com.veadan.folib.dto.ArtifactUploadAdapterJfrogDto.OriginalChecksums;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.utils.UserUtils;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.tika.Tika;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.activation.MimetypesFileTypeMap;
import javax.inject.Inject;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Collections;
import java.util.Date;
import java.util.Map;
import java.util.Optional;

/**
 * @author leipenghui
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog上传", tags = "JFrog上传")
public class ArtifactUploadController extends JFrogBaseController {
    @Inject
    private RepositoryManagementService repositoryManagementService;
    @Inject
    private ArtifactManagementService artifactManagementService;
    @Inject
    private ArtifactMetadataService artifactMetadataService;

    @Autowired
    private PromotionUtil promotionUtil;
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Inject
    private ArtifactRepository artifactRepository;
    @Inject
    private ArtifactPromotionProviderRegistry artifactPromotionProviderRegistry;
    @Inject
    @Lazy
    private MavenRepositoryFeatures mavenRepositoryFeatures;

    @Value("${folib.temp}")
    private String tempPath;

    @PreAuthorize("authenticated")
    @PutMapping(value = "/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<?> upload(@PathVariable String repositoryId,
                                    @PathVariable String artifactPath,
                                    @RequestParam(value = "uuid", required = false) String uuid,
                                    @RequestParam(value = "metaData", required = false) String metaData,
                                    HttpServletRequest request) throws Exception {
        String storageId = getDefaultStorageId(repositoryId);
        boolean checkRepository = checkRepository(storageId, repositoryId);
        if (!checkRepository) {
            return repositoryNotFound();
        }
        final InputStream inputStream = request.getInputStream();
        final String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");
        final String fileDownUrl = String.format("%s/artifactory/%s/%s", baseUrl, repositoryId, artifactPath);
        final String userName = UserUtils.getUsername();

        final ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, inputStream,
                repositoryPathResolver, artifactManagementService, promotionUtil,
                layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures,
                tempPath, artifactPath, metaData, uuid, null);
        final String msg = artifactUploadTask.call();
        if (StringUtils.isNotBlank(msg)) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(msg);
        }

        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        final Map<String, String> checksums = Optional.ofNullable(repositoryPath.getArtifactEntry().getChecksums()).orElse(Collections.emptyMap());
        final String sha256 = checksums.get("SHA-256");
        final ArtifactUploadAdapterJfrogDto artifactUploadAdapterJfrogDto = new ArtifactUploadAdapterJfrogDto();
        artifactUploadAdapterJfrogDto.setRepo(repositoryId);
        artifactUploadAdapterJfrogDto.setPath(artifactPath);
        artifactUploadAdapterJfrogDto.setCreated(DateUtil.format(new Date(), "yyyy-MM-dd HH:mm:ss"));
        artifactUploadAdapterJfrogDto.setCreatedBy(userName);
        artifactUploadAdapterJfrogDto.setDownloadUri(fileDownUrl);
        MimetypesFileTypeMap mimetypesFileTypeMap = new MimetypesFileTypeMap();
        String mimeType = mimetypesFileTypeMap.getContentType(RepositoryFiles.relativizePath(repositoryPath));
        artifactUploadAdapterJfrogDto.setMimeType(mimeType);
        artifactUploadAdapterJfrogDto.setSize(Files.size(repositoryPath) + "");
        artifactUploadAdapterJfrogDto.setChecksums(new Checksums()
                .setMd5(checksums.get("MD5"))
                .setSha1(checksums.get("SHA-1"))
                .setSha256(sha256));
        artifactUploadAdapterJfrogDto.setOriginalChecksums(new OriginalChecksums().setSha256(sha256));
        artifactUploadAdapterJfrogDto.setUri(fileDownUrl);

        return ResponseEntity.ok(JSONUtil.toJsonStr(JSONUtil.parse(artifactUploadAdapterJfrogDto), 2));
    }
}
