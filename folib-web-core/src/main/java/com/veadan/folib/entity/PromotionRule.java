package com.veadan.folib.entity;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

/**
 * 联邦晋级规则;
 *
 * @author :pj
 * @date : 2024-11-21
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(schema = "promotion_rule")
public class PromotionRule implements Serializable {

    /**
     * id
     */
    @Id
    @Column(name = "rule_id")
    private long ruleId;
    /**
     * 策略ID
     */
    @Column(name = "policy_id")
    private long policyId;
    /**
     * 规则类型:[path, metadata]
     */
    @Column(name = "rule_type")
    private String ruleType;
    /**
     * 属性key
     */
    @Column(name = "attribute_key")
    private String attributeKey;
    /**
     * 属性值
     */
    @Column(name = "attribute_value")
    private String attributeValue;
    /**
     * 更新时间
     */
    @Column(name = "update_time")
    private Date updateTime;
    /**
     * 创建时间
     */
    @Column(name = "create_time")
    private Date createdTime;


}