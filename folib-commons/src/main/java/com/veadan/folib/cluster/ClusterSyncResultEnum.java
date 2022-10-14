package com.veadan.folib.cluster;

public enum ClusterSyncResultEnum {

    SUCCESS(200,"ok"),
    FAIL(500,"error");

    private Integer code;

    private String message;

    ClusterSyncResultEnum() {
    }

    ClusterSyncResultEnum(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }
}
