package com.veadan.folib.mapper;

import com.veadan.folib.entity.AffectedVersionAttributionEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Mapper
public interface AffectedVersionAttributionMapper {

    /**
     * 创建受影响的版本归属
     *
     * @param attribution 受影响的版本归属
     */
    @Transactional(rollbackFor = Exception.class)
    int createAffectedVersionAttribution(AffectedVersionAttributionEntity attribution);

    /**
     * 更新受影响的版本归属
     *
     * @param attribution 受影响的版本归属
     */
    @Transactional(rollbackFor = Exception.class)
    int updateAffectedVersionAttribution(  AffectedVersionAttributionEntity attribution);

    @Transactional(rollbackFor = Exception.class)
    int insertOrUpdateBatch(@Param("entities") List<AffectedVersionAttributionEntity> entities);
}
