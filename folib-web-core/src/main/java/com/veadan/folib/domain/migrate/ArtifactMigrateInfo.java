package com.veadan.folib.domain.migrate;

import com.veadan.folib.constant.GlobalConstants;
import lombok.Data;
import org.apache.commons.lang3.StringUtils;

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

    private Integer syncMeta;

    public void setBrowsePrefix(String browsePrefix){
        this.browsePrefix= StringUtils.removeEnd(browsePrefix, GlobalConstants.SEPARATOR);
    }

    public void setRemotePreUrl(String remotePreUrl){
        this.remotePreUrl= StringUtils.removeEnd(remotePreUrl, GlobalConstants.SEPARATOR);
    }


}
