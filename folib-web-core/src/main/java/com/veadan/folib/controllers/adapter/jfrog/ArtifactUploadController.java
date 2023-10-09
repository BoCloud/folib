package com.veadan.folib.controllers.adapter.jfrog;
import cn.hutool.core.date.DateTime;
import com.alibaba.fastjson.JSON;
import com.veadan.folib.dto.ArtifactUploadAdapterJfrogDto.OriginalChecksums;
import com.veadan.folib.dto.ArtifactUploadAdapterJfrogDto.Checksums;

import com.veadan.folib.components.promotion.ArtifactPromotionProviderRegistry;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.dto.ArtifactUploadAdapterJfrogDto;
import com.veadan.folib.promotion.ArtifactUploadTask;
import com.veadan.folib.promotion.PromotionUtil;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.repository.MavenRepositoryFeatures;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.inject.Inject;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Date;

/**
 * @author leipenghui
 */
@Slf4j
@RequestMapping("/artifactory")
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@Api(description = "JFrog上传", tags = "JFrog上传")
public class ArtifactUploadController extends BaseController 
{
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
    @PutMapping(value = "/{storageId}/{repositoryId}/{artifactPath:.+}")
    public ResponseEntity<?> upload(@PathVariable String storageId,
                                    @PathVariable String repositoryId,
                                    @PathVariable String artifactPath,
                                    @RequestParam(value = "uuid", required = false) String uuid,
                                    @RequestHeader HttpHeaders httpHeaders, 
                                    HttpServletRequest request, 
                                    HttpServletResponse response) throws Exception
    {

        final ServletInputStream inputStream = request.getInputStream();
        final String fileName = artifactPath.replaceAll(".*/(.*)", "$1");
        final byte[] fileBytes = inputStream.readAllBytes();
        final MultipartFile file = new MockMultipartFile(fileName, fileBytes);
        final String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/");

        final String fileDownUrl = String.format("%s%s", baseUrl, artifactPath);
        String metaData = null;

        final ArtifactUploadTask artifactUploadTask = new ArtifactUploadTask(storageId, repositoryId, file,
                repositoryManagementService, repositoryPathResolver, artifactManagementService, promotionUtil, 
                layoutProviderRegistry, artifactMetadataService, artifactRepository, mavenRepositoryFeatures, 
                tempPath, artifactPath, metaData, uuid, null);
        final String msg = artifactUploadTask.call();
        if (StringUtils.isNotBlank(msg))
        { return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(msg); }
        
        final ArtifactUploadAdapterJfrogDto artifactUploadAdapterJfrogDto = new ArtifactUploadAdapterJfrogDto();
        artifactUploadAdapterJfrogDto.setRepo(repositoryId);
        artifactUploadAdapterJfrogDto.setPath(artifactPath);
        artifactUploadAdapterJfrogDto.setCreated(new Date().toString());
        artifactUploadAdapterJfrogDto.setCreatedBy("admin");
        artifactUploadAdapterJfrogDto.setDownloadUri(fileDownUrl);
        artifactUploadAdapterJfrogDto.setMimeType("");
        artifactUploadAdapterJfrogDto.setSize(String.valueOf(fileBytes.length));
        artifactUploadAdapterJfrogDto.setChecksums(new Checksums());
        artifactUploadAdapterJfrogDto.setOriginalChecksums(new OriginalChecksums());
        artifactUploadAdapterJfrogDto.setUri(fileDownUrl);
        
        return ResponseEntity.ok(JSON.toJSONString(artifactUploadAdapterJfrogDto, true));
    }
}
