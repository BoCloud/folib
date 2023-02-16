package com.veadan.folib.controllers.ahzw;


import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.enums.ProductTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.nio.file.Files;
import java.util.Set;

@RestController
@RequestMapping("/api/artifact/folib/dependentLibrary")
@Api(value = "/api/artifact/folib/dependentLibrary")
public class DependentLibraryController extends BaseController {


    @Autowired
    private ClusterSyncService clusterSyncService;

    @Autowired
    private StorageManagementService storageManagementService;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    private RepositoryManagementService repositoryManagementService;

    @Value("${folib.dependentLibraryPrefix:anhui}")
    private String dependentLibraryPrefix;


    @PostMapping("/create")
    @ApiOperation(value = "供应商视角创建本地库与组合库", notes = "供应商视角创建本地库与组合库")
    public ResponseEntity createRepo(@RequestParam("storageId") String storageId,
                                     @RequestParam("repositoryId") String repositoryId,
                                     @RequestParam("layoutName") String layout,
                                     @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        // api接⼝：参数是project_id,system_id, layout=maven|npm type
        // 创建本地库   创建 组合库
        String localRepoName = repositoryId + "-local";
        String groupRepoName = repositoryId + "-group";
        String folibLibraryName = ProductTypeEnum.queryFolibLibraryByName(layout);
        if (StringUtils.isBlank(folibLibraryName)) {
            return getBadRequestResponseEntity("layout :" + layout + "不支持", accept);
        }

        try {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (null == storage) {
                // 创建存储空间
                StorageDto storageDto = new StorageDto(storageId, Set.of("admin"));
                storageManagementService.createStorage(storageDto);
                // 向其他集群节点同步storage
                SyncStorageDto syncStorageDto = new SyncStorageDto(storageDto, storageId, SyncStorageEnum.CREATE);
                clusterSyncService.syncStorage(syncStorageDto);

            }
            //创建local 库
            Repository repository = repositoryManagementService.getStorage(storageId).getRepository(localRepoName);
            if (null == repository) {
                RepositoryDto repositoryDto = new RepositoryDto(localRepoName);
                repositoryDto.setPolicy("mixed");
                repositoryDto.setStorageProvider("local");
                repositoryDto.setLayout(folibLibraryName);
                repositoryDto.setType("hosted");
                repositoryDto.setStatus("In Service");
                configurationManagementService.saveRepository(storageId, repositoryDto);
                RepositoryDto repoDto = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(localRepoName);

                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repositoryDto));
                if (!Files.exists(repositoryPath)) {
                    repositoryManagementService.createRepository(storageId, localRepoName);
                }
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repoDto, storageId, localRepoName, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
            }
            // 创建组合库
            Repository groupRepository = repositoryManagementService.getStorage(storageId).getRepository(groupRepoName);
            if (null == groupRepository) {
                RepositoryDto repositoryDto = new RepositoryDto(groupRepoName);
                repositoryDto.setPolicy("mixed");
                repositoryDto.setStorageProvider("local");
                repositoryDto.setLayout(folibLibraryName);
                repositoryDto.setType("group");
                repositoryDto.setStatus("In Service");

                // folib-common: 预制的代理库
                String groupRepoProxy = StringUtils.lowerCase("folib-common:" + dependentLibraryPrefix + "-" + layout + "-proxy");
                String groupRepoLocal = storageId + ":" + localRepoName;

                repositoryDto.setGroupRepositories(Set.of(groupRepoLocal, groupRepoProxy));
                configurationManagementService.saveRepository(storageId, repositoryDto);
                RepositoryDto repoDto = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(groupRepoName);

                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repositoryDto));
                if (!Files.exists(repositoryPath)) {
                    repositoryManagementService.createRepository(storageId, groupRepoName);
                }
                SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repoDto, storageId, groupRepoName, SyncRepositoryEnum.ADD_OR_UPDATE);
                clusterSyncService.syncRepository(syncRepositoryDto);
            }

        } catch (Exception e) {
            logger.error("依赖库创建失败 [{} {}] {}", storageId, repositoryId, e.getStackTrace());
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "依赖库创建失败", e, accept);
        }
        return getSuccessfulResponseEntity("ok", accept);
    }

    public ResponseEntity queryRepoArtifacte(@RequestParam("storageId") String storageId,
                                     @RequestParam("repositoryId") String repositoryId,
                                     @RequestParam("layoutName") String layout,
                                     @RequestHeader(HttpHeaders.ACCEPT) String accept) {

        // 返回仓库制品列表页



        return null;
    }


}
