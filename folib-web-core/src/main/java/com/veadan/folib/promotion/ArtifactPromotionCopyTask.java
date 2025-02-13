package com.veadan.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.repository.Repository;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.util.concurrent.Callable;

/**
 * 制品copy 任务
 *
 * @author qijianping
 */
@Slf4j
public class ArtifactPromotionCopyTask implements Callable<String> {

    private PromotionUtil promotionUtil;

    @Getter
    private RepositoryPath path;

    @Getter
    private Repository targetRepository;

    @Getter
    private Repository srcRepository;

    @Getter
    private RepositoryPath targetPath;

    public ArtifactPromotionCopyTask() {
    }

    public ArtifactPromotionCopyTask(RepositoryPath path, Repository srcRepository,RepositoryPath targetPath, Repository targetRepository) {
        this.path = path;
        this.srcRepository = srcRepository;
        this.targetRepository = targetRepository;
        this.promotionUtil = SpringUtil.getBean(PromotionUtil.class);
        this.targetPath = targetPath;
    }

    @Override
    public String call() {
        String rs = "";
        try {
            promotionUtil.handleFastCopy(path, srcRepository, targetPath,targetRepository);
            log.info("Copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] finished", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path);
        } catch (Exception e) {
            log.info("Copy srcRepository [{}] [{}] targetRepository [{}] [{}] path [{}] error [{}]", srcRepository.getStorage().getId(), srcRepository.getId(), targetRepository.getStorage().getId(), targetRepository.getId(), path, ExceptionUtils.getStackTrace(e));
            rs = e.getMessage();
        }
        return rs;
    }
}
