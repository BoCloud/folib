package com.veadan.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author leipenghui
 * @date 2024/7/18
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "storage_monitoring")
@ApiModel("StorageMonitoring")
public class StorageMonitoring implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @Id
    @ApiModelProperty("主键ID")
    @Column(name = "id")
    private Long id;

    /**
     * 存储空间名称
     */
    @ApiModelProperty("存储空间名称")
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 文件存储方式 local、s3
     */
    @ApiModelProperty("文件存储方式")
    @Column(name = "storage_provider")
    private String storageProvider;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    @Column(name = "repository_id")
    private String repositoryId;

    /**
     * 仓库类型
     */
    @ApiModelProperty("仓库类型")
    @Column(name = "repository_type")
    private String repositoryType;

    /**
     * 仓库布局
     */
    @ApiModelProperty("仓库布局")
    @Column(name = "repository_layout")
    private String repositoryLayout;

    /**
     * 仓库子布局
     */
    @ApiModelProperty("仓库子布局")
    @Column(name = "repository_sub_layout")
    private String repositorySubLayout;

    /**
     * 存储路径
     */
    @ApiModelProperty("存储路径")
    @Column(name = "storage_path")
    private String storagePath;

    /**
     * 制品大小
     */
    @ApiModelProperty("制品大小")
    @Column(name = "artifacts_size")
    private BigDecimal artifactsSize;

    /**
     * 制品数量
     */
    @ApiModelProperty("制品数量")
    @Column(name = "artifacts_count")
    private Long artifactsCount;

    /**
     * 文件大小
     */
    @ApiModelProperty("文件大小")
    @Column(name = "files_size")
    private BigDecimal filesSize;

    /**
     * 文件数量
     */
    @ApiModelProperty("文件数量")
    @Column(name = "files_count")
    private Long filesCount;

    /**
     * 文件使用存储占比
     */
    @ApiModelProperty("文件使用存储占比")
    @Column(name = "used_files_size_percentage")
    private BigDecimal usedFilesSizePercentage;

    /**
     * 文件夹数量
     */
    @ApiModelProperty("文件夹数量")
    @Column(name = "folders_count")
    private Long foldersCount;

    /**
     * 条目数量
     */
    @ApiModelProperty("条目数量")
    @Column(name = "items_count")
    private Long itemsCount;

    /**
     * 存储空间配额大小
     */
    @ApiModelProperty("存储空间配额大小")
    @Column(name = "storage_quota_size")
    private BigDecimal storageQuotaSize;

    /**
     * 已使用存储空间配额大小占比
     */
    @ApiModelProperty("已使用存储空间配额大小占比")
    @Column(name = "used_storage_quota_size_percentage")
    private BigDecimal usedStorageQuotaSizePercentage;

    /**
     * 其他文件大小
     */
    @ApiModelProperty("其他文件大小")
    @Column(name = "other_files_size")
    private BigDecimal otherFilesSize;

    /**
     * 其他文件使用存储占比
     */
    @ApiModelProperty("其他文件使用存储占比")
    @Column(name = "used_other_files_size_percentage")
    private BigDecimal usedOtherFilesSizePercentage;

    /**
     * 存储空间数量
     */
    @ApiModelProperty("存储空间数量")
    @Column(name = "storage_count")
    private Integer storageCount;

    /**
     * 仓库数量
     */
    @ApiModelProperty("仓库数量")
    @Column(name = "repository_count")
    private Integer repositoryCount;

    /**
     * 制品下载量
     */
    @ApiModelProperty("制品下载量")
    @Column(name = "artifacts_downloaded_count")
    private Long artifactsDownloadedCount;

    /**
     * 存储设备名称
     */
    @ApiModelProperty("存储设备名称")
    @Column(name = "storage_device_name")
    private String storageDeviceName;

    /**
     * 存储设备类型
     */
    @ApiModelProperty("存储设备类型")
    @Column(name = "storage_device_type")
    private String storageDeviceType;

    /**
     * 存储设备总容量
     */
    @ApiModelProperty("存储设备总容量")
    @Column(name = "storage_device_size")
    private BigDecimal storageDeviceSize;

    /**
     * 存储设备可用大小
     */
    @ApiModelProperty("存储设备可用大小")
    @Column(name = "storage_device_usable_size")
    private BigDecimal storageDeviceUsableSize;

    /**
     * 存储设备已使用大小
     */
    @ApiModelProperty("存储设备已使用大小")
    @Column(name = "used_storage_device_size")
    private BigDecimal usedStorageDeviceSize;

    /**
     * 已使用存储设备大小占比
     */
    @ApiModelProperty("已使用存储设备大小占比")
    @Column(name = "used_storage_device_size_percentage")
    private BigDecimal usedStorageDeviceSizePercentage;

    /**
     * 数据类型 1 仓库 2 回收站 3 存储空间 4 平台 5 存储设备
     */
    @ApiModelProperty("数据类型")
    @Column(name = "data_type")
    private Integer dataType;

    /**
     * 是否是最新数据 1 是 0 否
     */
    @ApiModelProperty("是否是最新数据")
    @Column(name = "is_latest")
    private Boolean isLatest;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @Column(name = "update_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;
}

