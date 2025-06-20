package com.veadan.folib.dto.component;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;

/**
 * 组件vo
 *
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ComponentTableDto implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * nativeId
     */
    private Long nativeId;
    /**
     * uuid
     */
    private String uuid;
    /**
     * 创建时间
     */
    private String created;
    /**
     * 最后更新时间
     */
    private String lastUpdated;
    /**
     * 组件名称
     */
    private String name;
    /**
     * 文件名
     */
    private String fileName;
    /**
     * 版本
     */
    private String version;
    /**
     * 组
     */
    private String groupId;
    /**
     * 描述
     */
    private String description;
    /**
     * pkg地址
     */
    private String purl;
    /**
     * url
     */
    private String url;
    /**
     * 通用平台枚举
     */
    private String cpe;
    /**
     * 通用平台枚举
     */
    private String md5sum;
    /**
     * 通用平台枚举
     */
    private String sha1sum;
    /**
     * 通用平台枚举
     */
    private String sha256sum;
    /**
     * license
     */
    private Set<String> license;
    /**
     * 漏洞
     */
    private Set<String> vulnerabilities;
    /**
     * 漏洞数量
     */
    private Integer vulnerabilitiesCount;
    /**
     * 严重的漏洞数量
     */
    private Integer criticalVulnerabilitiesCount;
    /**
     * 高危的漏洞数量
     */
    private Integer highVulnerabilitiesCount;
    /**
     * 中危的漏洞数量
     */
    private Integer mediumVulnerabilitiesCount;
    /**
     * 低危的漏洞数量
     */
    private Integer lowVulnerabilitiesCount;
    /**
     * 被封存的漏洞数量
     */
    private Integer suppressedVulnerabilitiesCount;

}
