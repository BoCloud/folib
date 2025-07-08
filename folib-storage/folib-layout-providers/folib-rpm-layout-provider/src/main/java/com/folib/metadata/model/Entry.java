package com.folib.metadata.model;

import lombok.Data;

import java.io.Serializable;

@Data
public class Entry implements Serializable {
    public String name;
    public String flags;
    public String epoch;
    public String version;
    public String release;
    public String pre;

    public Entry() {
    }
}
