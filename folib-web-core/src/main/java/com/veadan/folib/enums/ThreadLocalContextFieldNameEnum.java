package com.veadan.folib.enums;

import lombok.Getter;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 12:16
 * @since x.x.x
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
