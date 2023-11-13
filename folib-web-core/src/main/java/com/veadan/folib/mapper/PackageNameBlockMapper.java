package com.veadan.folib.mapper;

import com.veadan.folib.entity.PackageNameBlock;
import com.veadan.folib.scanner.common.base.CommonMapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface PackageNameBlockMapper extends CommonMapper<PackageNameBlock> {

    /**
     * 批量保存包名阻断数据
     *
     * @param packageNameBlockList 包名阻断列表
     */
    void batchInsertPackageNameBlock(@Param("packageNameBlockList") List<PackageNameBlock> packageNameBlockList);

}
