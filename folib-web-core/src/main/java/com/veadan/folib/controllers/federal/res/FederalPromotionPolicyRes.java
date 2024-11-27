package com.veadan.folib.controllers.federal.res;

import lombok.*;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class FederalPromotionPolicyRes {

    private long policyId;
    /**
     * 联邦晋级策略名
     */
    private String name;
    /**
     * 是否开启策略
     */
    private Boolean isEnabled;
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

    /**
     * 路径规则
     */
    private List<PromotionRuleRes> pathRules = new ArrayList<>();

    /**
     * 元数据规则
     */
    private List<PromotionRuleRes> metadataRules = new ArrayList<>();
    /**
     * 源仓库列表
     */
    private List<FederalRepositoryRes> sourceRepositories = new ArrayList<>();
    /**
     * 目标仓库列表
     */
    private List<FederalRepositoryRes> targetRepositories = new ArrayList<>();
}
