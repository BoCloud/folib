package com.veadan.folib.services;

import com.veadan.folib.dto.externalnode.ExternalNodeDto;
import com.veadan.folib.dto.externalnode.ExternalNodeRepositoryDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author veadan
 **/
public interface ExternalNodeService {

    /**
     * 查询外部节点分页列表
     *
     * @param page             页码
     * @param limit            每页大小
     * @param externalNodeForm 表单参数
     * @return 外部节点分页列表
     */
    TableResultResponse<ExternalNodeDto> queryExternalNodeList(Integer page, Integer limit, ExternalNodeDto externalNodeForm);

    /**
     * 查询外部节点
     *
     * @param externalNodeForm 表单参数
     * @return 外部节点
     */
    ExternalNodeDto getExternalNode(ExternalNodeDto externalNodeForm);

    /**
     * 保存外部节点
     *
     * @param externalNodeForm 表单参数
     */
    void saveExternalNode(ExternalNodeDto externalNodeForm);

    /**
     * 更新外部节点
     *
     * @param externalNodeForm 表单参数
     */
    void updateExternalNode(ExternalNodeDto externalNodeForm);

    /**
     * 删除外部节点
     *
     * @param id id
     */
    void deleteExternalNode(Long id);

    /**
     * 外部节点仓库列表
     *
     * @param type 仓库类型
     * @return 外部节点仓库列表
     */
    List<ExternalNodeRepositoryDto> getExternalNodeRepositories(String type);
}
