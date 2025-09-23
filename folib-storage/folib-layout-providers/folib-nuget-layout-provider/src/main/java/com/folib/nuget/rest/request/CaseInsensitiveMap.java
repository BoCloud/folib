package com.folib.nuget.rest.request;

import java.util.HashMap;




public class CaseInsensitiveMap extends HashMap<String, String> {
    public String put(String key, String value) {
        return (String)super.put(key.toLowerCase(), value);
    }

    public String get(Object key) {
        return (String)super.get(key.toString().toLowerCase());
    }
}
