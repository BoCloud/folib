package com.veadan.folib.controllers.federal.req;

import lombok.*;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;


@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class FederalPromotionPolicyUpdateReq extends FederalPromotionPolicyBaseReq {

    /**
     * id
     */
    private long policyId;
    /**
     * 更新时间
     */
    private Date updateTime;

    /**
     * 更新人
     */
    private String updatedBy;

    /**
     /**
     * 源端仓库列表
     */
    private List<FederalRepositoryUpdateReq> sourceRepositories;

    /**
     * 目标端仓库列表
     */
    private List<FederalRepositoryUpdateReq> targetRepositories;

    /**
     * 路径规则列表
     */
    private List<PromotionRuleUpdateReq> pathRules;
    /**
     * 元数据规则列表
     */
    private List<PromotionRuleUpdateReq> metadataRules;
}
