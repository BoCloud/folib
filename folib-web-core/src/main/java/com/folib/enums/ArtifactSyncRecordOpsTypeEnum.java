package com.folib.enums;

/**
 * 制品同步记录-操作类型
 * @author veadan
 * @date 2023/10/10 14:32
 */
public enum ArtifactSyncRecordOpsTypeEnum 
{
    PROMOTION(1, "晋级"),
    DISPATCH(2, "分发"),
    ;

    ArtifactSyncRecordOpsTypeEnum(Integer val, String desc) {
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
