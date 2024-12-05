package com.veadan.folib.domain.policy;

import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyCreateReq;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyQueryReq;
import com.veadan.folib.controllers.federal.req.FederalPromotionPolicyUpdateReq;
import com.veadan.folib.controllers.federal.res.FederalPromotionPolicyRes;
import com.veadan.folib.controllers.federal.res.FederalRepositoryRes;
import com.veadan.folib.domain.policy.dto.SyncArtifatDTO;
import com.veadan.folib.entity.FederalPromotionPolicy;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Page;

import java.util.List;

public interface FederalPromotionPolicyService {

    /**
     * 新增联邦晋级策略
     *
     * @param createReq 新增策略参数
     */
    void addPolicy(FederalPromotionPolicyCreateReq createReq);

    /**
     * 删除联邦晋级策略
     *
     * @param policyId 策略id
     */
    void deletePolicy(long policyId);

    /**
     * 编辑联邦晋级策略
     * @param req 编辑参数
     */
    void editPolicy(FederalPromotionPolicyUpdateReq req);

    /**
     * 分页查询
     *
     * @param queryReq 筛选条件
     * @return 查询结果
     */
    Page<FederalPromotionPolicyRes> paginQuery(FederalPromotionPolicyQueryReq queryReq);

    /**
     * 查看联邦晋级策略详情
     *
     * @param policyId 策略id
     * @return FederalPromotionPolicyRes
     */
    FederalPromotionPolicyRes policyDetail(long policyId);

    /**
     * 重置旧数据
     */
    void restOldData();

    /**
     * 根据存储空间id和仓库id查询
     * @param storageId  存储空间ID
     * @param repositoryId 仓库ID
     * @return FederalRepositoryRes
     */
    List<FederalRepositoryRes> queryByStorageIdAndRepositoryId(String storageId, String repositoryId);

    /**
     * 联邦制品删除同步
     * @param syncArtifatDTOS 同步参数
     */
    void federalDeleteArtifatSync(List<SyncArtifatDTO> syncArtifatDTOS);
}
