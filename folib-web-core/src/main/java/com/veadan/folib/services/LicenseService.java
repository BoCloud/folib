package com.veadan.folib.services;

import com.veadan.folib.entity.License;
import com.veadan.folib.forms.component.ComponentTableForm;
import com.veadan.folib.forms.license.LicenseTableForm;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface LicenseService {

    /**
     * 新增许可证
     *
     * @param license 许可证
     */
    void saveLicense(License license);

    /**
     * 修改许可证
     *
     * @param license 许可证
     */
    void updateLicense(License license);

    /**
     * 查询许可证
     *
     * @param license 许可证
     * @return 许可证列表
     */
    List<License> selectLicense(License license);

    /**
     * 查询许可证
     *
     * @param license 许可证
     * @return 许可证
     */
    License selectOneLicense(License license);

    /**
     * 更新许可证地址
     */
    void updateLicenseUrl();

    /**
     * 更新许可证中文内容
     */
    void updateContentCn();

    /**
     * 分页查询license列表
     *
     * @param page          页码
     * @param limit         每页数量
     * @param searchKeyword 搜索关键词
     * @return license列表
     */
    TableResultResponse<LicenseTableForm> queryLicensePage(Integer page, Integer limit, String searchKeyword);
}
