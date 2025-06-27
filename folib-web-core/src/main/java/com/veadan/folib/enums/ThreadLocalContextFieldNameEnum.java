package com.veadan.folib.enums;

import lombok.Getter;

/**
 * @author veadan
 * @date 2023/12/6 12:16
 */
public enum ThreadLocalContextFieldNameEnum {
    ARTIFACT_DISPATCH_SYNC_NO("ARTIFACT_DISPATCH_SYNC_NO", "制品分发上下文字段名称-同步编号");

    private String fieldName;
    private String desc;

    ThreadLocalContextFieldNameEnum(String fieldName, String desc) {
        this.fieldName = fieldName;
        this.desc = desc;
    }

    public String getFieldName() {
        return fieldName;
    }

    public String getDesc() {
        return desc;
    }
}
