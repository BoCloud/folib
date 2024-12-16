package com.veadan.folib.controllers.federal.req;

import lombok.*;
import lombok.experimental.Accessors;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class PromotionRuleUpdateReq  extends PromotionRuleBaseReq{


    private long ruleId;
    /**
     * 策略ID
     */
    private long policyId;
}
