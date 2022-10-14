package com.veadan.folib.scanner.enums;

import lombok.AllArgsConstructor;
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
    CRITICAL("CRITICAL", "严重"),
    /**
     * 高危
     */
    HIGH("HIGH", "高危"),
    /**
     * 中危
     */
    MEDIUM("MEDIUM", "中危"),
    /**
     * 低危
     */
    LOW("LOW", "低危"),
    ;


    /**
     * 漏洞严重程度类型
     */
    private String type;
    /**
     * 漏洞严重程度名称
     */
    private String title;

    public static String queryTitleByType(String type) {
        String title = "";
        for (SeverityTypeEnum severityTypeEnum : SeverityTypeEnum.values()) {
            if (severityTypeEnum.type.equals(type)) {
                title = severityTypeEnum.getTitle();
                break;
            }
        }
        return title;
    }

}
