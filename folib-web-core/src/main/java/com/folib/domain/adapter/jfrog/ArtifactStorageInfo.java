package com.folib.domain.adapter.jfrog;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.Map;

/**
 * @author veadan
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactStorageInfo {

    /**
     * 仓库
     */
    private String repo;

    /**
     * 制品路径
     */
    private String path;

    /**
     * 创建时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date created;

    /**
     * 创建人
     */
    private String createdBy;

    /**
     * 修改时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date lastModified;

    /**
     * 修改人
     */
    private String modifiedBy;

    /**
     * 更新时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date lastUpdated;

    /**
     * 下载链接
     */
    private String downloadUri;

    /**
     * 文件类型
     */
    private String mimeType;

    /**
     * 文件大小
     */
    private String size;

    /**
     * 文件校验信息
     */
    private Map<String, String> checksums;

    /**
     * 文件校验信息
     */
    private Map<String, String> originalChecksums;

    /**
     * 信息链接
     */
    private String uri;

    /**
     * 元数据
     */
    private Map<String, Object> properties;
}
