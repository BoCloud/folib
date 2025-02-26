package com.veadan.folib.cron.jobs;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.veadan.folib.artifact.coordinates.RawArtifactCoordinates;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.event.privilege.PrivilegeEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collection;
import java.util.Objects;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author huayanjun
 * @since 2025-02-10 15:21
 */

@Slf4j
@Component
public class ReplicationBackup {

    @Resource
    private ArtifactResolutionService artifactResolutionService;

    @Resource
    private RepositoryPathResolver repositoryPathResolver;

    @Resource

    private ArtifactManagementService artifactManagementService;

    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private ClusterSyncService clusterSyncService;

    @Lazy
    @Resource
    private PrivilegeEventListenerRegistry privilegeEventListenerRegistry;

    @Resource
    private RepositoryManagementService repositoryManagementService;

    @Value("${folib.temp}")
    private String tempPath;

    public void backUpByPath(Repository repository, Collection<String> paths, String path) {
        // 将增量文件复制到指定目录下
        String prefixPath=tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId()+"/"+path;
        if (Files.exists(Path.of(prefixPath))) {
            try {
                Files.delete(Path.of(prefixPath));
            } catch (IOException e) {
                log.error("delete replication backup failed", e);
            }
        }
        for (String p : paths) {
            RepositoryPath scrPath = repositoryPathResolver.resolve(repository, p);
            Path targetPath = Path.of(prefixPath + p);
            try {
                if(!Files.exists(targetPath)) {
                    Files.createDirectories(targetPath.getParent());
                }
                Files.copy(scrPath, targetPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
            } catch (IOException e) {
                log.info("{}文件备份失败",p);
            }
        }
        String src = genZip(repository,Path.of(prefixPath));
        if( Objects.nonNull(src)) {
            saveZip(repository,src);
        }else {
            log.info("unable to backup path {}", path);
        }
    }


    // 先保存zip文件
    private String genZip(Repository repository,Path targetPath) {
        String dateStr= DateUtil.format(DateUtil.date(), DatePattern.PURE_DATETIME_PATTERN)+".zip";
        String prefixPath=tempPath + "/replication/" + repository.getStorage().getId() + "/" + repository.getId()+"/"+dateStr;
        if(!Files.exists(targetPath)){
            log.info("备份文件不存在");
            return null;
        }
        try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(prefixPath));Stream<Path> pathStream = Files.walk(targetPath)) {
            pathStream.forEach(path -> {
                        String zipEntryName = targetPath.relativize(path).toString().replace("\\", "/");
                        try {
                            if (Files.isDirectory(path)) {
                                zos.putNextEntry(new ZipEntry(zipEntryName + "/")); // 保证目录结构
                                zos.closeEntry();
                            } else {
                                zos.putNextEntry(new ZipEntry(zipEntryName));
                                Files.copy(path, zos);
                                zos.closeEntry();
                            }
                        } catch (IOException e) {
                            log.info("打包异常");
                        }
                    });
        } catch (Exception e) {
            log.info("操作异常{}", e.getMessage(), e);
            return null;
        }
        return prefixPath;
    }

    void saveZip(Repository repository,String src) {
        // 查找备份的仓库是否存在
        String backupName = repository.getId() + "_backup";
        String storageId = repository.getStorage().getId();
        Repository backupRepo = repository.getStorage().getRepository(backupName);
        try {
            if (Objects.isNull(backupRepo)) {
                //创建一个本地的仓库
                RepositoryDto newRepo = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId())
                        .getRepository(repository.getId());
                newRepo.setId(backupName);
                newRepo.setLayout(RawArtifactCoordinates.LAYOUT_NAME);
                newRepo.setType(RepositoryTypeEnum.HOSTED.getType());
                newRepo.setRemoteRepository(null);
                newRepo.setSubLayout("raw");
                newRepo.setAllowsDeployment(true);
                newRepo.setAllowsRedeployment(true);
                newRepo.setProxyConfiguration(null);
                configurationManagementService.saveRepository(newRepo.getStorage().getId(), newRepo);
                backupRepo = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId())
                        .getRepository(backupName);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(backupRepo);
                try {
                    if (!Files.exists(repositoryPath)) {
                        repositoryManagementService.createRepository(storageId, backupRepo.getId());
                    }
                } catch (Exception ex) {
                    log.error("Failed to create the repository path {}!", backupName, ex);
                    try {
                        configurationManagementService.removeRepository(storageId, backupName);
                    } catch (Exception e) {
                        log.error("Failed to remove the repository {}!",  backupRepo.getId(), e);
                    }
                    throw new RuntimeException(ex.getMessage());
                }

                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(newRepo, storageId, backupName, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                //同步资源信息到其他节点
                privilegeEventListenerRegistry.dispatchResourceSyncEvent(storageId + "_" + backupName);
            }
            String path=src.substring(src.lastIndexOf("/")+1);
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(backupRepo,path );
            artifactManagementService.validateAndStore(repositoryPath, Path.of(src));
        } catch (Exception e) {
            log.error("备份存储异常");
        }
    }
}
