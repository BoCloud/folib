package com.veadan.folib.scanner.mapper;

import com.veadan.folib.common.base.CommonMapper;
import com.veadan.folib.scanner.entity.ScanRules;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @version 2022-06-03 14:51:22
 * @email xuxinping@126.com
 */
@Component
public interface ScanRulesMapper extends CommonMapper<ScanRules> {

    /**
     * 统计properties表数据量，若小于等于1，初始化漏洞数据
     *
     * @return 数据量
     */
    int countProperties();
}
