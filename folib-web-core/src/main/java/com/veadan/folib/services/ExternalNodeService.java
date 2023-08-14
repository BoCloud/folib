package com.veadan.folib.services;

import com.veadan.folib.forms.externalnode.ExternalNodeForm;
import com.veadan.folib.forms.externalnode.ExternalNodeRepositoryForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
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
    TableResultResponse<ExternalNodeForm> queryExternalNodeList(Integer page, Integer limit, ExternalNodeForm externalNodeForm);

    /**
     * 查询外部节点
     *
     * @param externalNodeForm 表单参数
     * @return 外部节点
     */
    ExternalNodeForm getExternalNode(ExternalNodeForm externalNodeForm);

    /**
     * 保存外部节点
     *
     * @param externalNodeForm 表单参数
     */
    void saveExternalNode(ExternalNodeForm externalNodeForm);

    /**
     * 更新外部节点
     *
     * @param externalNodeForm 表单参数
     */
    void updateExternalNode(ExternalNodeForm externalNodeForm);

    /**
     * 删除外部节点
     *
     * @param id id
     */
    void deleteExternalNode(Long id);

    /**
     * 外部节点仓库列表
     * @return 外部节点仓库列表
     */
    List<ExternalNodeRepositoryForm> getExternalNodeRepositories();
}
