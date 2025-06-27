package com.veadan.folib.task;


import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.components.artifact.ArtifactComponent;
import com.veadan.folib.components.thirdparty.foeyes.FoEyesComponent;
import com.veadan.folib.components.thirdparty.foeyes.enums.UploadStatusEnum;
import com.veadan.folib.domain.bom.Bom;
import com.veadan.folib.domain.bom.FoEyes;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.scanner.entity.ScanRules;
import com.veadan.folib.scanner.mapper.ScanRulesMapper;
import com.veadan.folib.scanner.service.ScanRulesService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;


import javax.inject.Inject;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author veadan
 * bom重传task
 */
@Slf4j
@Component
@EnableScheduling
public class BomUploadTask {

    @Inject
    @Lazy
    private ScanRulesService scanRulesService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    @Lazy
    private DistributedLockComponent distributedLockComponent;

    @Inject
    @Lazy
    private ArtifactComponent artifactComponent;

    @Inject
    @Lazy
    private FoEyesComponent foEyesComponent;

    @Scheduled(cron = "0 0/5 * * * ? ")
    public void run() {
        log.info("BomUploadTask starting time [{}]", DateUtil.now());
        if (!foEyesComponent.enable()) {
            log.info("foEyes not enable bomUploadTask skip...");
            return;
        }
        String lockName = "BomUploadTask";
        long waitTime = 3L;
        log.info("Wait for the lock [{}]", lockName);
        if (distributedLockComponent.lock(lockName, waitTime)) {
            try {
                log.info("Locked for [{}]", lockName);
                List<ScanRules> scanRulesList = scanRulesService.queryBomOnScanList();
                if (CollectionUtils.isEmpty(scanRulesList)) {
                    return;
                }
                for (ScanRules scanRules : scanRulesList) {
                    try {
                        RepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(scanRules.getStorage(), scanRules.getRepository());
                        try (Stream<Path> pathStream = Files.walk(rootRepositoryPath)) {
                            pathStream.filter(item -> {
                                try {
                                    RepositoryPath itemRepositoryPath = (RepositoryPath) item;
                                    return !RepositoryFiles.isHidden(itemRepositoryPath) && RepositoryFiles.isArtifact(itemRepositoryPath) && Files.exists(artifactComponent.getBomRepositoryPath(itemRepositoryPath));
                                } catch (Exception ex) {
                                    log.error("Bom upload handler path [{}] error [{}]", item.toString(), ExceptionUtils.getStackTrace(ex));
                                }
                                return false;
                            }).forEach(path -> {
                                try {
                                    RepositoryPath repositoryPath = (RepositoryPath) path;
                                    RepositoryPath bomRepositoryPath = artifactComponent.getBomRepositoryPath(repositoryPath);
                                    if (Files.exists(bomRepositoryPath)) {
                                        Bom bom = JSONObject.parseObject(Files.readString(bomRepositoryPath), Bom.class);
                                        if (Objects.nonNull(bom)) {
                                            FoEyes foEyes = bom.getFoEyes();
                                            boolean upload = Objects.isNull(foEyes) || !UploadStatusEnum.UPLOAD_SUCCESS.getType().equals(foEyes.getUploadStatus());
                                            if (upload) {
                                                //将SBOM传给foeyes
                                                foEyesComponent.bomUpload(repositoryPath, bomRepositoryPath, bom);
                                            }
                                        }
                                    }
                                } catch (Exception ex) {
                                    log.error("Bom upload handler path [{}] error [{}]", path.toString(), ExceptionUtils.getStackTrace(ex));
                                }
                            });
                        }
                    } catch (Exception ex) {
                        log.error("Bom upload handler storageId [{}] repositoryId [{}] error [{}]", scanRules.getStorage(), scanRules.getRepository(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            } finally {
                distributedLockComponent.unLock(lockName);
            }
        } else {
            log.info("LockName [{}] was not get lock", lockName);
        }
        log.info("BomUploadTask end time [{}]", DateUtil.now());
    }
}
