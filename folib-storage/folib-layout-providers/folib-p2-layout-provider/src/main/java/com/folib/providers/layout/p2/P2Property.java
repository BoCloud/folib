package com.folib.providers.layout.p2;


import jakarta.xml.bind.annotation.XmlAttribute;

public class P2Property
{

    private String name;

    private String value;

    @XmlAttribute
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @XmlAttribute
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}
