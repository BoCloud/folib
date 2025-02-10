package com.veadan.folib.cron.jobs;

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
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Collection;
import java.util.Objects;
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

    @Resource
    private PrivilegeEventListenerRegistry privilegeEventListenerRegistry;

    public void backUpByPath(Repository repository, Collection<String> paths, String path) {
        try (ByteArrayInputStream inputStream = genZip(paths, repository)) {
            saveZip(repository, inputStream, path);
        } catch (Exception e) {
            log.error("备份异常");
        }
    }


    private ByteArrayInputStream genZip(Collection<String> paths, Repository repository) {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        try (ZipOutputStream zipOut = new ZipOutputStream(byteArrayOutputStream)) {
            for (String filePath : paths) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, filePath);
                try (InputStream inputStream = artifactResolutionService.getInputStream(repositoryPath)) {
                    String name = filePath.substring(filePath.lastIndexOf("/"));
                    ZipEntry zipEntry = new ZipEntry(name);
                    zipOut.putNextEntry(zipEntry);
                    byte[] buffer = new byte[1024];
                    int len;
                    while ((len = inputStream.read(buffer)) > 0) {
                        zipOut.write(buffer, 0, len);
                    }
                    zipOut.closeEntry();
                }
            }
            return new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
        } catch (Exception e) {
            log.info("操作异常{}", e.getMessage(), e);
            return null;
        }
    }

    void saveZip(Repository repository, ByteArrayInputStream byteArrayInputStream, String path) {
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
                configurationManagementService.saveRepository(newRepo.getStorage().getId(), newRepo);
                backupRepo = configurationManagementService.getMutableConfigurationClone().getStorage(repository.getStorage().getId())
                        .getRepository(backupName);
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(newRepo, storageId, backupName, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
                //同步资源信息到其他节点
                privilegeEventListenerRegistry.dispatchResourceSyncEvent(storageId + "_" + backupName);
            }
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(backupRepo, path);
            artifactManagementService.validateAndStore(repositoryPath, byteArrayInputStream);
        } catch (Exception e) {
            log.error("备份存储异常");
        }
    }
}
