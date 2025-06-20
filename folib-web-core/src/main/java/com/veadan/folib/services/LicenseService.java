package com.veadan.folib.services;

import com.veadan.folib.domain.license.LicenseBlackWhite;
import com.veadan.folib.entity.License;
import com.veadan.folib.dto.license.LicenseTableDto;
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
     * @param page           页码
     * @param limit          每页数量
     * @param searchKeyword  搜索关键词
     * @param licenseId      license名称
     * @param blackWhiteType 黑白名单类型
     * @return license列表
     */
    TableResultResponse<LicenseTableDto> queryLicensePage(Integer page, Integer limit, String searchKeyword, String licenseId, Integer blackWhiteType);

    /**
     * 查询license列表
     *
     * @param searchKeyword         搜索关键词
     * @param licenseId             license名称
     * @param blackWhiteType        黑白名单类型
     * @param excludeBlackWhiteType 排除黑白名单类型
     * @return license列表
     */
    List<LicenseTableDto> queryLicense(String searchKeyword, String licenseId, Integer blackWhiteType, Integer excludeBlackWhiteType);

    /**
     * 设置黑白名单
     *
     * @param licenseBlackWhite 参数
     */
    void blackWhite(LicenseBlackWhite licenseBlackWhite);

    /**
     * 获取license缓存
     *
     * @return license缓存
     */
    List<License> getLicenseCache();
}
