package com.veadan.folib.services;

import com.veadan.folib.domain.PackageNameBlockInfo;
import com.veadan.folib.entity.PackageNameBlock;
import com.veadan.folib.dto.packagenameblock.PackageNameBlockDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface PackageNameBlockService {

    /**
     * 查询包名阻断分页列表
     *
     * @param page                 页码
     * @param limit                每页大小
     * @param packageNameBlockForm 表单参数
     * @return 包名阻断分页列表
     */
    TableResultResponse<PackageNameBlockInfo> queryPackageNameBlockList(Integer page, Integer limit, PackageNameBlockDto packageNameBlockForm);

    /**
     * 保存包名阻断
     *
     * @param packageNameBlockForm 表单参数
     */
    void savePackageNameBlock(PackageNameBlockDto packageNameBlockForm);

    /**
     * 更新包名阻断
     *
     * @param packageNameBlockForm 表单参数
     */
    void updatePackageNameBlock(PackageNameBlockDto packageNameBlockForm);

    /**
     * 删除包名阻断
     *
     * @param packageNameBlockForm 表单参数
     */
    void deletePackageNameBlock(PackageNameBlockDto packageNameBlockForm);

    /**
     * 包名阻断
     *
     * @param packageNameBlockForm 表单参数
     * @return 包名阻断
     */
    PackageNameBlockInfo selectOnePackageNameBlock(PackageNameBlockDto packageNameBlockForm);

    /**
     * 解析配置信息到数据库
     */
    void parseConfig();

    /**
     * 获取包名阻断缓存
     * @return 包名阻断缓存
     */
    List<PackageNameBlock> getPackageNameBlockCache();
}
