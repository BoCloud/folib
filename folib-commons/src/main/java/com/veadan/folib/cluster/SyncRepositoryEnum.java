package com.veadan.folib.cluster;

public enum SyncRepositoryEnum {
    ADD_OR_UPDATE(1),
    DELETE(2);

    private int type;

    public int getType() {
        return type;
    }

    SyncRepositoryEnum(int type) {
        this.type = type;
    }
}
