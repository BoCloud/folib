package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 制品元数据类型枚举
 *
 * @author veadan
 * @date 2022/11/29
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum ArtifactMetadataEnum {

    /**
     * 数字
     */
    NUMERICAL("数字"),
    /**
     * 字符串
     */
    STRING("字符串"),
    /**
     * 文本
     */
    TEXT("文本"),
    /**
     * Markdown
     */
    MD("Markdown"),
    /**
     * json
     */
    JSON("JSON"),
    ;

    /**
     * 类型
     */
    private String type;
}
