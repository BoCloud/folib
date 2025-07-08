package com.folib.enums;

/**
 * 制品同步记录-同步模式
 * @author veadan
 * @date 2023/10/10 14:32
 */
public enum ArtifactSyncRecordSyncModelEnum 
{
    PUSH(1, "推"),
    PULL(2, "拉"),
    ;

    ArtifactSyncRecordSyncModelEnum(Integer val, String desc) {
        this.val = val;
        this.desc = desc;
    }

    public Integer getVal() {
        return val;
    }

    public String getDesc() {
        return desc;
    }

    private Integer val;
    private String desc;
}
