package com.veadan.folib.mapper;

import com.veadan.folib.entity.VulnerableSoftwareEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Mapper
public interface VulnerableSoftwareMapper {

    /**
     * 通过 CPE v2.3 字符串返回 VulnerableSoftware。
     * @param cpe23 CPE 2.3 字符串
     * @return 一个 VulnerableSoftware 对象，如果未找到，则为 null
     */
    VulnerableSoftwareEntity getVulnerableSoftwareByCpe23(String cpe23,
                                                          String versionEndExcluding, String versionEndIncluding,
                                                          String versionStartExcluding, String versionStartIncluding);

    /**
     * 批量插入或更新 VulnerableSoftware 对象
     * @param vsList VulnerableSoftware 对象列表
     */
    @Transactional(rollbackFor = Exception.class)
    int insertOrUpdateBatch( @Param("entities")List<VulnerableSoftwareEntity> vsList);
}
