package com.veadan.folib.controllers.cluster;


import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.services.StorageManagementService;
import io.swagger.annotations.Api;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
//@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/cluster/")
@Api(value = "/api/configuration/cluster/")
public class FolibClusterSyncController extends BaseController {
    private static final Logger logger = LoggerFactory.getLogger(FolibClusterSyncController.class);

    @Autowired
    private StorageManagementService storageManagementService;

    @PostMapping("syncStorage")
    public ResponseEntity syncStorage(@RequestBody SyncStorageDto syncStorageDto) {
        try {
            if (syncStorageDto.getSycnStorageEnum().getType() == 1) {
                storageManagementService.createStorage(syncStorageDto.getStorageDto());
                logger.info("sycn create storage [{}] success", syncStorageDto.getStorageId());
            } else if (syncStorageDto.getSycnStorageEnum().getType() == 2) {
                storageManagementService.updateStorage(syncStorageDto.getStorageDto());
                logger.info("sycn update storage [{}] success", syncStorageDto.getStorageId());
            } else if (syncStorageDto.getSycnStorageEnum().getType() == 3) {
                configurationManagementService.removeStorage(syncStorageDto.getStorageId());
                logger.info("sycn remove storage [{}] success", syncStorageDto.getStorageId());
            }
        } catch (Exception e) {
            logger.error("sync storage error {}", e.getMessage());
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync storage ok");
    }

    @PostMapping("syncRepository")
    public ResponseEntity syncRepository(@RequestBody SyncRepositoryDto syncRepositoryDto) {
        try {
            if (syncRepositoryDto.getSycnRepositoryEnum().getType() == 1) {
                configurationManagementService.saveRepository(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryDto());
                logger.info("sycn save repository success");
            } else if (syncRepositoryDto.getSycnRepositoryEnum().getType() == 2) {
                configurationManagementService.removeRepository(syncRepositoryDto.getStorageId(),
                        syncRepositoryDto.getRepositoryId());
                logger.info("sycn remove repository success");
            }
        } catch (Exception e) {
            logger.error("sync repository error {}", e.getMessage());
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync repository ok");
    }


}
