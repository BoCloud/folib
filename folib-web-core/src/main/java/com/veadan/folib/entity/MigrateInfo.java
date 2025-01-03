package com.veadan.folib.entity;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author huayanjun
 * @since 2024-12-31 20:36
 */
@Data
@Table(name = "migrate_info")
public class MigrateInfo {

    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;
    /**
     * 存储空间
     */
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 仓库
     */
    @Column(name = "repository_id")
    private String repositoryId;

    @Column(name = "layout")
    private String layout;

    /**
     * 迁移id
     */
    @Column(name = "migrate_id")
    private String migrateId;

    /**
     * jfrog
     */
    @Column(name = "migrate_type")
    private String migrateType;

    /**
     * 迁移状态
     */
    @Column(name = "sync_status")
    private Integer syncStatus;

    /**
     * 制品总数
     */
    @Column(name = "total_artifact")
    private Integer totalArtifact;

    /**
     * 迁移成功量
     */
    @Column(name = "success_mount")
    private Integer successMount;

    /**
     * 原制品大小
     */
    @Column(name = "used_space")
    private String usedSpace;

    @Column(name = "sync_dir_path")
    private String syncDirPath;

    @Column(name = "sync_property")
    private Integer syncProperty;


    public String getStorageIdAndRepositoryId(){
        return this.storageId+":"+this.repositoryId;
    }


}
