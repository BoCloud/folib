package com.veadan.folib.cluster;

public enum SyncClusterDispatchEnum {
    ADD_OR_UPDATE(1),
    DELETE(2);
    private int type;

    public int getType() {
        return type;
    }

    SyncClusterDispatchEnum(int type) {
        this.type = type;
    }
}
