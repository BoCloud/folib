package com.veadan.folib.gremlin.entity;

import lombok.AllArgsConstructor;
import lombok.Data;

/**
 * @Author: haifeng
 * @Date: 2019/11/26 15:18
 */

public class KeyValue {
    private String key;
    private String value;

    public KeyValue(String key, String value) {
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
