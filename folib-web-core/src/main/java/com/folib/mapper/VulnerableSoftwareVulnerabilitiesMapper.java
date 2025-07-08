package com.folib.mapper;

import com.folib.entity.VulnerableSoftwareVulnerabilitiesEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Mapper
public interface VulnerableSoftwareVulnerabilitiesMapper {

    /**
     * 批量插入或更新
     *
     * @param entities
     */
    @Transactional(rollbackFor = Exception.class)
    int insertOrUpdateBatch(@Param("entities") List<VulnerableSoftwareVulnerabilitiesEntity> entities);
}
