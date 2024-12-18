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
public class FederalPromotionPolicyCreateReq extends FederalPromotionPolicyBaseReq {

    /**
     * 创建人
     */
    private String createdBy;
    /**
    /**
     * 源端仓库列表
     */
    private List<FederalRepositoryCreateReq> sourceRepositories;

    /**
     * 目标端仓库列表
     */
    private List<FederalRepositoryCreateReq> targetRepositories;

    /**
     * 路径规则列表
     */
    private List<PromotionRuleCreateReq> pathRules;

    /**
     * 元数据规则列表
     */
    private List<PromotionRuleCreateReq> metadataRules;

}
