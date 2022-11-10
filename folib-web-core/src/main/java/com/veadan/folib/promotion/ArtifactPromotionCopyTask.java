package com.veadan.folib.promotion;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.storage.repository.Repository;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

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
    private String path;

    @Getter
    private Repository destRepository;

    @Getter
    private Repository srcRepository;

    public ArtifactPromotionCopyTask() {
    }

    public ArtifactPromotionCopyTask(String path, Repository destRepository, Repository srcRepository) {
        this.path = path;
        this.destRepository = destRepository;
        this.srcRepository = srcRepository;
        this.promotionUtil = SpringUtil.getBean(PromotionUtil.class);
    }

    @Override
    public String call() {
        String rs = "";
        try {
            if (path.startsWith("s3://")) {
                promotionUtil.handleS3ArtifactCopy(path, destRepository, srcRepository);
            } else {
                promotionUtil.handleCopy(path, destRepository, srcRepository);
            }
            log.info("Artifact copyed [{}]", path);
        } catch (Exception e) {
            log.error("ArtifactPromotionCopyTask Exception {} ", e.getMessage());
            rs = e.getMessage();
        }
        return rs;
    }
}
