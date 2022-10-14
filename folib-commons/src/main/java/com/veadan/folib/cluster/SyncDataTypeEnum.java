package com.veadan.folib.cluster;

public enum SyncDataTypeEnum {

    STORAGE(1),
    REPOSITORY(2);

    private Integer value;

    SyncDataTypeEnum(Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return value;
    }
}
