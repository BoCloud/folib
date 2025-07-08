package com.folib.scanner.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.scanner.entity.ScanRules;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @version 2022-06-03 14:51:22
 * @email xuxinping@126.com
 */
@Component
public interface ScanRulesMapper extends BaseMapper<ScanRules> {

    /**
     * 统计properties表数据量，若小于等于1，初始化漏洞数据
     *
     * @return 数据量
     */
    int countProperties();
}
