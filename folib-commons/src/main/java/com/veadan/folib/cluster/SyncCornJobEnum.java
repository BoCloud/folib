package com.veadan.folib.cluster;

public enum SyncCornJobEnum {
    ADD_OR_UPDATE(1),
    DELETE(2);
    private int type;

    public int getType() {
        return type;
    }

    SyncCornJobEnum(int type) {
        this.type = type;
    }
}
