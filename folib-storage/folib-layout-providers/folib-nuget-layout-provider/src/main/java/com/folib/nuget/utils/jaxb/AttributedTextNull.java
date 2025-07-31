package com.folib.nuget.utils.jaxb;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;
import lombok.Data;


import java.io.Serializable;




@XmlAccessorType(XmlAccessType.FIELD)
@Data
public class AttributedTextNull implements Serializable {
    @XmlValue
    private final String value;
    @XmlAttribute(
            name = "null",
            namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
    )
    private final String attribute;

    public AttributedTextNull() {
        this.attribute = AttributeTypeEnum.TRUE.type;
        this.value = null;
    }

    public AttributedTextNull(String value) {
        this.attribute = AttributeTypeEnum.TRUE.type;
        this.value = value;
    }
}
