package com.veadan.folib.constant;

/**
 * 制品同步记录-同步状态
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/10 14:32
 * @since x.x.x
 */
public enum ArtifactSyncRecordStatusEnum 
{
    READY(1, "就绪"),
    IN_SYNC(2, "同步中"),
    SUCCESS(3, "成功"),
    FAILED(4, "失败"),
    ;

    ArtifactSyncRecordStatusEnum(Integer val, String desc) {
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
