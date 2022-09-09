package com.veadan.folib.scanner.enums;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 * @date 2022/9/8
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SeverityTypeEnum {

    /**
     * 严重
     */
    CRITICAL("CRITICAL"),
    /**
     * 高危
     */
    HIGH("HIGH"),
    /**
     * 中危
     */
    MEDIUM("MEDIUM"),
    /**
     * 低危
     */
    LOW("LOW"),
    ;


    /**
     * 漏洞严重程度类型
     */
    private String type;
}
