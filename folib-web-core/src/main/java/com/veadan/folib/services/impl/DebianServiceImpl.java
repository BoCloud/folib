package com.veadan.folib.services.impl;

import cn.hutool.core.lang.UUID;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinatesAdapter;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.domain.DebianMetadata;
import com.veadan.folib.domain.debian.DebianParserVO;
import com.veadan.folib.domain.debian.DebianUploadBO;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DeltaIndexEventType;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.event.DebianIndexEvent;
import com.veadan.folib.indexer.DebianIncrementalIndexer;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.DebianService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.mockito.internal.util.collections.Sets;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.commons.CommonsMultipartFile;

import javax.annotation.Resource;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Set;

/**
 * @author huayanjun
 * @since 2024-09-06 11:21
 */
@Slf4j
@Component
public class DebianServiceImpl implements DebianService {

    @Value("${folib.temp}")
    private String tempPath;

    @Resource
    private ArtifactManagementService artifactManagementService;

    @Resource
    private RepositoryPathResolver repositoryPathResolver;

    @Resource
    private DebianArtifactCoordinatesAdapter debianArtifactCoordinatesAdapter;

    @Resource
    private DebianIncrementalIndexer debianIncrementalIndexer;

    @Resource
    private DictService dictService;


    public DebianParserVO parseArtifact(String storageId, String repositoryId, MultipartFile file) {
        String fileOriginalName = ((CommonsMultipartFile) file).getFileItem().getName();
        DebianParserVO artifactParse = new DebianParserVO();
        String parentPath  = tempPath + File.separator + "parseArtifact" + File.separator;
        String artifactPath = parentPath + fileOriginalName;
        File artifactFile = new File(artifactPath);

        try {
            if (!artifactFile.exists()) {
                boolean dirCreated = artifactFile.mkdirs();
                if (!dirCreated) {
                    throw new IOException("Failed to create directory: " + parentPath);
                }
            }
            file.transferTo(artifactFile);
        } catch (Exception e) {
            throw new RuntimeException("制品解析失败" + e.getMessage());
        }
        Assert.isTrue(fileOriginalName.endsWith(DebianConstant.DEFAULT_EXTENSION), "无效的文件类型");
        try (InputStream extraMeta = Files.newInputStream(Paths.get(artifactFile.getAbsolutePath()))) {
            DebianMetadata extract = DebianUtils.extract(extraMeta);
            //Assert.notNull(extract, "无效的制品");
            if(extract !=null){
                artifactParse.setVersion(extract.getVersion());
                artifactParse.setArchitecture(extract.getArchitecture());
                artifactParse.setFileName(extract.getPackageName());
            }
            artifactParse.setPath(artifactFile.getAbsolutePath());
            return artifactParse;
        } catch (Exception e) {
            log.error("extra metadata failed{},", e.getMessage(), e);
            throw new RuntimeException("制品解析失败:" + e.getMessage());
        }

    }

    public String upload(DebianUploadBO uploadBO) {
        // 目录
        Path temArtifact = Path.of(uploadBO.getPath());
        DebianArtifactCoordinates coordinates = DebianArtifactCoordinates.of(uploadBO.getComponent(), temArtifact.getFileName().toString(), DebianConstant.DEFAULT_EXTENSION);
        coordinates.setDistribution(uploadBO.getDistribution());
        coordinates.setArchitecture(uploadBO.getArchitecture());
        coordinates.setVersion(uploadBO.getVersion());
        coordinates.setFileName(uploadBO.getFileName());
        String path = coordinates.convertToPath(coordinates);
        log.info("制品路径为{}", path);

        try (InputStream is = Files.newInputStream(temArtifact)) {
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(uploadBO.getStorageId(), uploadBO.getRepositoryId(), path);
            DebianUtils.setCoordinates(repositoryPath, coordinates);
            artifactManagementService.store(repositoryPath, is);
            Files.delete(temArtifact);
            // 异步更新package
            DebianIndexEvent addEvent = DebianUtils.generateEvent(coordinates, repositoryPath.getArtifactEntry(), DeltaIndexEventType.ADD);
            debianIncrementalIndexer.index(repositoryPath.getRepository(), Sets.newSet(addEvent));
        } catch (Exception e) {
            log.error("制品上传异常{}", e.getMessage(), e);
            throw new RuntimeException("制品上传异常:" + e.getMessage());
        }
        return "upload success";
    }

    @Async
    public void batchUpload(List<MultipartFile> files, String storageId, String repositoryId, String distribution, String component) {

        Repository repository = null;
        Set<DebianIndexEvent> events = Sets.newSet();
        for (MultipartFile file : files) {
            File tempFile = null;
            try {
                String fileName = file.getOriginalFilename();
                tempFile = File.createTempFile("upload-", "-" + file.getOriginalFilename());
                file.transferTo(tempFile);
                log.info("begin to execute file {}", fileName);
                try (InputStream is = new FileInputStream(tempFile)) {
                    DebianMetadata extract = DebianUtils.extract(is);
                    Assert.notNull(extract, "valid artifact ");
                    DebianArtifactCoordinates coordinate = DebianArtifactCoordinates.of(component, fileName, DebianConstant.DEFAULT_EXTENSION);
                    coordinate.setVersion(extract.getVersion());
                    coordinate.setArchitecture(extract.getArchitecture());
                    coordinate.setFileName(extract.getPackageName());
                    coordinate.setDistribution(distribution);
                    String path = coordinate.convertToPath(coordinate);
                    log.info("artifact path is {}", path);
                    RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
                    repository = repositoryPath.getRepository();
                    DebianUtils.setCoordinates(repositoryPath, coordinate);
                    try (InputStream artifactStream = new FileInputStream(tempFile)) {
                        artifactManagementService.store(repositoryPath, artifactStream);
                    }
                    DebianIndexEvent addEvent = DebianUtils.generateEvent(coordinate, repositoryPath.getArtifactEntry(), DeltaIndexEventType.ADD);
                    log.info("file {} upload success", fileName);
                    events.add(addEvent);
                    updateUploadProcess(fileName, "100");
                }
            } catch (Exception e) {
                updateUploadProcess(file.getOriginalFilename(), "0");
                log.error("create file:{} failed", file.getOriginalFilename(), e);
            } finally {
                log.info("begin delete temp file");
                if(tempFile!= null){
                    tempFile.delete();
                }
            }
        }

        if (repository != null) {
            log.info("begin update packages and release");
            debianIncrementalIndexer.index(repository, events);
        }
    }

    public void updateUploadProcess(String fileName, String percent) {
        String uuid = UUID.fastUUID().toString();
        dictService.saveOrUpdateDict(Dict.builder().dictType(DictTypeEnum.UPLOAD_PROCESS.getType()).dictKey(uuid).dictValue(percent).alias(fileName).build(), false);
    }


}
