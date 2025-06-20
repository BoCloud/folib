package com.veadan.folib.enums;

public enum NpmSubLayout {

    NPM("npm"),
    YARN("yarn"),
    OHPM("ohpm");

    private String value;

    NpmSubLayout(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
}
