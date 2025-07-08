package com.folib.event.index;

import lombok.Getter;

@Getter
public enum IndexTypeEnum {

    RPM("rpm"),

    debian("debian");

    private String type;


    IndexTypeEnum(String type) {
        this.type = type;
    }

}
