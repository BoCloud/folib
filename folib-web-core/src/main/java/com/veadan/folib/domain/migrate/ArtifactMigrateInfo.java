package com.veadan.folib.domain.migrate;

import lombok.Data;

import java.time.LocalDate;

/**
 * @author huayanjun
 * @since 2024-12-24 10:56
 */
@Data
public class ArtifactMigrateInfo {
    // 迁移id
    private String migrateId;
    // 迁移仓库数
    private int total;
    // 浏览器前缀
    private String browsePrefix;
    // 单实例并发仓库数
    private int batchSize ;
    // 单仓库迁移线程数 （默认cpu核心数*2）
    private int threadNumber;
    // 迁移状态 0-初始
    private int status;

    private String username;

    private String password;

    private String remotePreUrl;

    public void increaseCount(){
        this.total++;
    }

}
