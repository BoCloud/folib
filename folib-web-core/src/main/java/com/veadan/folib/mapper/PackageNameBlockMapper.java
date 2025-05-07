package com.veadan.folib.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.veadan.folib.entity.PackageNameBlock;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * @author leipenghui
 */
@Component
public interface PackageNameBlockMapper extends BaseMapper<PackageNameBlock> {

    /**
     * 批量保存包名阻断数据
     *
     * @param packageNameBlockList 包名阻断列表
     */
    void batchInsertPackageNameBlock(@Param("packageNameBlockList") List<PackageNameBlock> packageNameBlockList);

}
