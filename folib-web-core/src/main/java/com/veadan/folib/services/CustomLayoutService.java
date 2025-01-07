package com.veadan.folib.services;

import com.veadan.folib.domain.customlayout.CustomLayoutRecord;
import com.veadan.folib.entity.CustomLayout;
import com.veadan.folib.forms.customlayout.CustomLayoutForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface CustomLayoutService {


    /**
     * 查询自定义布局分页列表
     *
     * @param page             页码
     * @param limit            每页大小
     * @param customLayoutForm 表单参数
     * @return 自定义布局分页列表
     */
    TableResultResponse<CustomLayoutRecord> queryCustomLayoutPage(Integer page, Integer limit, CustomLayoutForm customLayoutForm);

    /**
     * 查询自定义布局列表
     *
     * @param customLayoutForm 表单参数
     * @return 自定义布局分页列表
     */
    List<CustomLayoutRecord> queryCustomLayoutList(CustomLayoutForm customLayoutForm);

    /**
     * 查询自定义布局
     *
     * @param customLayout 自定义布局
     * @return 自定义布局
     */
    CustomLayoutForm queryCustomLayout(CustomLayout customLayout);

    /**
     * 新增自定义布局
     *
     * @param customLayoutForm 参数
     */
    void saveCustomLayout(CustomLayoutForm customLayoutForm);

    /**
     * 更新自定义布局
     *
     * @param customLayoutForm 参数
     */
    void updateCustomLayout(CustomLayoutForm customLayoutForm);

    /**
     * 删除自定义布局
     *
     * @param customLayout 参数
     */
    void deleteCustomLayout(CustomLayout customLayout);

    /**
     * 自定义布局
     *
     * @param customLayout 参数
     * @return 自定义布局
     */
    CustomLayout getCustomLayout(CustomLayout customLayout);
}
