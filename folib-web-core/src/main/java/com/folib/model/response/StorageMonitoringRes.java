package com.folib.model.response;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author veadan
 * @date 2024/7/18
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class StorageMonitoringRes implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @ApiModelProperty("主键ID")
    private Long id;

    /**
     * 存储空间名称
     */
    @ApiModelProperty("存储空间名称")
    private String storageId;

    /**
     * 文件存储方式 local、s3
     */
    @ApiModelProperty("文件存储方式")
    private String storageProvider;

    /**
     * 仓库名称
     */
    @ApiModelProperty("仓库名称")
    private String repositoryId;

    /**
     * 仓库类型
     */
    @ApiModelProperty("仓库类型")
    private String repositoryType;

    /**
     * 仓库布局
     */
    @ApiModelProperty("仓库布局")
    private String repositoryLayout;

    /**
     * 仓库子布局
     */
    @ApiModelProperty("仓库子布局")
    private String repositorySubLayout;

    /**
     * 存储路径
     */
    @ApiModelProperty("存储路径")
    private String storagePath;

    /**
     * 制品大小
     */
    @ApiModelProperty("制品大小")
    private BigDecimal artifactsSize;

    /**
     * 制品数量
     */
    @ApiModelProperty("制品数量")
    private Long artifactsCount;

    /**
     * 文件大小
     */
    @ApiModelProperty("文件大小")
    private BigDecimal filesSize;

    /**
     * 文件数量
     */
    @ApiModelProperty("文件数量")
    private Long filesCount;

    /**
     * 文件使用存储占比
     */
    @ApiModelProperty("文件使用存储占比")
    private BigDecimal usedFilesSizePercentage;

    /**
     * 文件夹数量
     */
    @ApiModelProperty("文件夹数量")
    private Long foldersCount;

    /**
     * 条目数量
     */
    @ApiModelProperty("条目数量")
    private Long itemsCount;

    /**
     * 存储空间配额大小
     */
    @ApiModelProperty("存储空间配额大小")
    private BigDecimal storageQuotaSize;

    /**
     * 已使用存储空间配额大小占比
     */
    @ApiModelProperty("已使用存储空间配额大小占比")
    private BigDecimal usedStorageQuotaSizePercentage;

    /**
     * 其他文件大小
     */
    @ApiModelProperty("其他文件大小")
    private BigDecimal otherFilesSize;

    /**
     * 其他文件使用存储占比
     */
    @ApiModelProperty("其他文件使用存储占比")
    private BigDecimal usedOtherFilesSizePercentage;

    /**
     * 存储空间数量
     */
    @ApiModelProperty("存储空间数量")
    private Integer storageCount;

    /**
     * 仓库数量
     */
    @ApiModelProperty("仓库数量")
    private Integer repositoryCount;

    /**
     * 制品下载量
     */
    @ApiModelProperty("制品下载量")
    private Long artifactsDownloadedCount;

    /**
     * 存储设备名称
     */
    @ApiModelProperty("存储设备名称")
    private String storageDeviceName;

    /**
     * 存储设备类型
     */
    @ApiModelProperty("存储设备类型")
    private String storageDeviceType;

    /**
     * 存储设备总容量
     */
    @ApiModelProperty("存储设备总容量")
    private BigDecimal storageDeviceSize;

    /**
     * 存储设备可用大小
     */
    @ApiModelProperty("存储设备可用大小")
    private BigDecimal storageDeviceUsableSize;

    /**
     * 存储设备已使用大小
     */
    @ApiModelProperty("存储设备已使用大小")
    private BigDecimal usedStorageDeviceSize;

    /**
     * 已使用存储设备大小占比
     */
    @ApiModelProperty("已使用存储设备大小占比")
    private BigDecimal usedStorageDeviceSizePercentage;

    /**
     * 数据类型 1 仓库 2 回收站 3 存储空间 4 平台 5 存储设备
     */
    @ApiModelProperty("数据类型")
    private Integer dataType;

    /**
     * 数据类型集合
     */
    @ApiModelProperty("数据类型集合")
    private List<Integer> dataTypes;

    /**
     * 是否是最新数据 1 是 0 否
     */
    @ApiModelProperty("是否是最新数据")
    private Boolean isLatest;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private String createDate;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private String createDay;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;
}

