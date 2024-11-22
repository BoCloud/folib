package com.veadan.folib.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "federal_promotion_policy")
public class FederalPromotionPolicy implements Serializable, Cloneable {
    /**
     * id
     */
    @Id
    private long policyId;
    /**
     * 联邦晋级策略名
     */
    private String name;
    /**
     * 是否开启策略
     */
    private String isEnabled;
    /**
     * 标签[default:标记为老数据适配，latest:标记新建的]
     */
    private String tag;
    /**
     * 创建时间
     */
    private Date createdTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 创建人
     */
    private String createdBy;
    /**
     * 更新人
     */
    private String updatedBy;

}
