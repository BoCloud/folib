package com.veadan.folib.cluster;

public enum SyncDataStatusEnum {
    WILL_EXECUTE_STATUS(0),
    COMPLETE_STATUS(1);
    private Integer status;

    SyncDataStatusEnum(Integer status) {
        this.status = status;
    }

    public Integer getStatus() {
        return status;
    }

    public void setStatus(Integer status) {
        this.status = status;
    }
}
