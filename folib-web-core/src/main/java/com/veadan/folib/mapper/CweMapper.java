package com.veadan.folib.mapper;

import com.veadan.folib.entity.CweEntity;
import com.veadan.folib.scanner.vulnerability.model.Cwe;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Mapper
public interface CweMapper {

    /**
     * 获取所有的CWE对象
     * @return a list of CWE objects
     */
    List<Cwe> getAllCwes();

    /**
     * 根据CWE标识获取CWE对象
     * @param cweId the CWE ID
     * @return a CWE object
     */
    Cwe getCweById(@Param("cweId") Integer cweId);


    /**
     * 插入CWE对象
     * @param id
     * @param name
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    int insert(Integer id, String name);

    /**
     * 批量插入CWE对象
     * @param entities
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    int insertOrUpdateBatch(@Param("entities") List<CweEntity> entities);
}
