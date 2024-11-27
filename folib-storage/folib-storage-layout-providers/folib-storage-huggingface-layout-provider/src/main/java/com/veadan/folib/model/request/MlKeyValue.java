package com.veadan.folib.model.request;

import java.beans.ConstructorProperties;
import java.util.Map;
import lombok.Generated;

public class MlKeyValue {
    private String key;

    private Map<String, String> value;

    @Generated
    public MlKeyValue() {}

    @ConstructorProperties({"key", "value"})
    @Generated
    public MlKeyValue(String key, Map<String, String> value) {
        this.key = key;
        this.value = value;
    }

    @Generated
    public void setKey(String key) {
        this.key = key;
    }

    @Generated
    public void setValue(Map<String, String> value) {
        this.value = value;
    }

    @Generated
    public String getKey() {
        return this.key;
    }

    @Generated
    public Map<String, String> getValue() {
        return this.value;
    }
}

