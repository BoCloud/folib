package com.veadan.folib.event.index;

import lombok.Getter;

@Getter
public enum IndexTypeEnum {

    RPM("rpm"),

    debian("debian"),

    CONDA("conda");

    private String type;


    IndexTypeEnum(String type) {
        this.type = type;
    }

}
