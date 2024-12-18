package com.veadan.folib.controllers.federal.req;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Id;
import java.util.Date;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class PromotionRuleBaseReq {

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
