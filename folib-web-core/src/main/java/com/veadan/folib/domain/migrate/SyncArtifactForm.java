package com.veadan.folib.domain.migrate;

import com.veadan.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SyncArtifactForm {

    /**
     * 同步类型
     */
    private String type;

    /**
     * 存储空间
     */
    @NotBlank(message = "存储空间不能为空")
    private String storageId;

    /**
     * 仓库名称
     */
    @NotBlank(message = "仓库名称不能为空")
    private String repositoryId;

    /**
     * 浏览地址
     */
    private String browseUrl;

    /**
     * 休眠毫秒数
     */
    private Integer sleepMillis;

    /**
     * dom
     */
    private String dom;

    /**
     * 每批数量
     */
    private Integer batch;

    /**
     * 索引路径
     */
    private String indexPath;

    /*
        最大线程数
     */
    private Integer maxThreadNum;


    private int totalArtifact;

    private int syncMount;

    private String migrateId;

    private String username;

    private String password;

    private Integer syncMeta;

    private String apiUrl;

    private JfrogPropertySyncer syncer;


    public String getStoreAndRepo(){
        return this.storageId+":"+repositoryId;
    }


}
