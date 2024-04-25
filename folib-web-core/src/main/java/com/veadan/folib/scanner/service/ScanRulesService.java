package com.veadan.folib.scanner.service;

import com.veadan.folib.domain.Tree;
import com.veadan.folib.scanner.entity.ScanRules;

import java.util.List;

/**
 * @author leipenghui
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
}
