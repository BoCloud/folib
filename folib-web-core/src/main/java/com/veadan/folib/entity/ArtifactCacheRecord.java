package com.veadan.folib.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.util.Date;

/**
 * @author leipenghui
 * @date 2023/10/27
 * 制品缓存记录
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "artifact_cache_record")
@ApiModel("ArtifactCacheRecord")
public class ArtifactCacheRecord {


    /**
     * 主键ID
     */
    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;

    /**
     * 节点ID
     */
    @ApiModelProperty("节点ID")
    @Column(name = "node_id")
    private String nodeId;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    private Date createTime;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @Column(name = "update_time")
    private Date updateTime;

    /**
     * 存储空间名称
     */
    @ApiModelProperty("存储空间名称")
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @Column(name = "repository_id")
    private String repositoryId;

    /**
     * 制品路径
     */
    @ApiModelProperty("制品路径")
    @Column(name = "artifact_path")
    private String artifactPath;

    /**
     * 制品路径前缀
     */
    @ApiModelProperty("制品路径前缀")
    @Column(name = "artifact_path_prefix")
    private String artifactPathPrefix;

    /**
     * 制品大小
     */
    @ApiModelProperty("制品大小")
    @Column(name = "size")
    private Long size;

    /**
     * MD5
     */
    @ApiModelProperty("MD5")
    @Column(name = "md5")
    private String md5;

    /**
     * SHA-1
     */
    @ApiModelProperty("SHA-1")
    @Column(name = "sha1")
    private String sha1;

    /**
     * SHA-256
     */
    @ApiModelProperty("SHA-256")
    @Column(name = "sha256")
    private String sha256;

    /**
     * 下载次数
     */
    @ApiModelProperty("下载次数")
    @Column(name = "download_count")
    private Long downloadCount;

    /**
     * 最后下载时间
     */
    @ApiModelProperty("最后下载时间")
    @Column(name = "latest_download_time")
    private Date latestDownloadTime;

    /**
     * 缓存目录
     */
    @ApiModelProperty("缓存目录")
    @Column(name = "cache_directory_path")
    private String cacheDirectoryPath;

    /**
     * 缓存路径
     */
    @ApiModelProperty("缓存路径")
    @Column(name = "cache_path")
    private String cachePath;
}