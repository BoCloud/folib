package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.License;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author veadan
 */
@Component
public interface LicenseMapper extends BaseMapper<License> {

    /**
     * 查询License列表
     *
     * @param searchKeyword         搜索关键词
     * @param licenseId             license名称
     * @param blackWhiteType        黑白名单类型
     * @param excludeBlackWhiteType 排除黑白名单类型
     * @param isDeprecated          是否已弃用 1是 0否
     * @return License列表
     */
    List<License> selectLicense(@Param("searchKeyword") String searchKeyword, @Param("licenseId") String licenseId, @Param("blackWhiteType") Integer blackWhiteType, @Param("excludeBlackWhiteType") Integer excludeBlackWhiteType, @Param("isDeprecated") Integer isDeprecated);


}
