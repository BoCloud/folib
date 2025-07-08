package com.folib.providers.layout.p2;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;

import java.util.ArrayList;
import java.util.List;

public class P2Properties
{

    private List<P2Property> propertites = new ArrayList<>();

    private Integer size;

    @XmlElement(name = "property")
    public List<P2Property> getPropertites() {
        return propertites;
    }

    public void setPropertites(List<P2Property> propertites) {
        this.propertites = propertites;
    }

    @XmlAttribute
    public Integer getSize() {
        return size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }
}
