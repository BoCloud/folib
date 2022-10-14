package com.veadan.folib.cluster;

public enum SyncStorageEnum {

    CREATE(1),
    UPDATE(2),
    DELETE(3);

    private int type;

    public int getType() {
        return type;
    }

    SyncStorageEnum(int type) {
        this.type = type;
    }
}
