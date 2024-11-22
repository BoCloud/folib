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
public class PromotionRule implements Serializable, Cloneable {

    /**
     * id
     */
    @Id
    private long ruleId;
    /**
     * 策略ID
     */
    private long policyId;
    /**
     * 规则类型:[path, metadata]
     */
    private String ruleType;
    /**
     * 属性key
     */
    private String attributeKey;
    /**
     * 属性值
     */
    private String attributeValue;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 创建时间
     */
    private Date createdTime;


}