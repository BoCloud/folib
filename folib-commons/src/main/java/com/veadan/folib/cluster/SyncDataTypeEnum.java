package com.veadan.folib.cluster;

public enum SyncDataTypeEnum {

    STORAGE(1),
    REPOSITORY(2),
    SECURITY_POLICY(3),
    METADATA(4);

    private Integer value;

    SyncDataTypeEnum(Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return value;
    }
}
