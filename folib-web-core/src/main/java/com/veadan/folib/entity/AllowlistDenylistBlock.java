package com.veadan.folib.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

/**
 * 黑白名单阻断
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "allowlist_denylist_block")
public class AllowlistDenylistBlock implements Serializable {

    private Long id;

    /**
     * 标识具体规则如漏洞ID
     */
    private String identifier;

    /**
     * 类型[whites:白名单,blacklist：黑名单]
     */
    private String type;

    /**
     * 分类 [vulnerability:漏洞,license:证书]
     */
    private String category;

    /**
     * 有效期
     */
    private Date validFrom;

    /**
     * '标签[default:标记为老数据适配，latest:标记新建的]',
     */
    private String tag;

    /**
     * 创建人
     */
    private String createdBy;

    /**
     * 创建时间
     */
    private Date createdTime;

    /**
     * 更新人
     */
    private String updatedBy;

    /**
     * 更新时间
     */
    private Date updateTime;
}
