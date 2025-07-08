package com.folib.scanner.service;

import com.folib.domain.Tree;
import com.folib.scanner.entity.ScanRules;

import java.util.List;

/**
 * @author veadan
 * @date 2024/4/22
 **/
public interface ScanRulesService {

    /**
     * 新增或更新扫描规则
     *
     * @param scanRules 扫描规则
     */
    void saveOrUpdateScanRules(ScanRules scanRules);

    /**
     * 查询开启Bom的仓库列表
     *
     * @return 开启Bom的仓库列表
     */
    List<ScanRules> queryBomOnScanList();

    /**
     * 查询开启Bom的仓库树结构
     *
     * @return 开启Bom的仓库树结构
     */
    List<Tree> queryBomOnScanTree();

    /**
     * 查询开启扫描的仓库列表
     *
     * @return 开启扫描的仓库列表
     */
    List<ScanRules> queryOnScanList();

    /**
     * 查询开启扫描的仓库树结构
     *
     * @return 开启扫描的仓库树结构
     */
    List<Tree> queryOnScanTree();

    /**
     * 根据扫描ID查询扫描规则
     *
     * @param scanId 扫描ID
     * @return 扫描规则
     */
    ScanRules findByScanId(String scanId);
}
