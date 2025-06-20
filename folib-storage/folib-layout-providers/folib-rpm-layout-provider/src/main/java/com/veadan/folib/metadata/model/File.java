package com.veadan.folib.metadata.model;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;

@Data
@AllArgsConstructor
public class File implements Serializable {
    public String type;
    public String path;

    public File() {
    }
}
