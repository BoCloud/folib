package com.veadan.folib.enums;


import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 安全级别枚举
 *
 * @author leipenghui
 * @date 2022/9/27
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SafeLevelEnum {

    /**
     * 初始状态
     */
    INIT("init"),
    /**
     * 未扫描
     */
    UN_SCAN("unScan"),
    /**
     * 扫描中
     */
    SCANNING("scanning"),
    /**
     * 扫描完成
     */
    SCAN_COMPLETE("scanComplete"),
    /**
     * 扫描失败
     */
    SCAN_FAIL("scanFail"),
    ;

    private String level;
}
