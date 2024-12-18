package com.veadan.folib.controllers.federal.res;

import lombok.*;
import lombok.experimental.Accessors;

import java.util.Date;



@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class PromotionRuleRes {

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
