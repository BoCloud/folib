package com.veadan.folib.cluster;

public enum SyncDataTypeEnum {

    STORAGE(1),
    REPOSITORY(2),
    SECURITY_POLICY(3),
    METADATA(4),
    REPOSITORY_JOB(5),
    CLUSTER_DISPATCH(6);

    private Integer value;

    SyncDataTypeEnum(Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return value;
    }
}
