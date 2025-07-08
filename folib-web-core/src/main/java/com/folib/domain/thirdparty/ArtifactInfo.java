package com.folib.domain.thirdparty;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * @author veadan
 **/
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ArtifactInfo {

    /**
     * 仓库
     */
    private String repo;

    /**
     * 制品路径
     */
    private String path;

    /**
     * 制品名称
     */
    private String name;

    /**
     * 下载地址
     */
    private String download;

    /**
     * 创建时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date created;

    /**
     * 更新时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updated;

    /**
     * 版本号
     */
    private String version;

    /**
     * 仓库格式
     */
    private String format;

    /**
     * 仓库类型
     */
    private String repoType;
}
