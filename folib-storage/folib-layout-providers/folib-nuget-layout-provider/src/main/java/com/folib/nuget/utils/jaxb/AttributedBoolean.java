package com.folib.nuget.utils.jaxb;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;
import lombok.Data;


import java.io.Serializable;



@XmlAccessorType(XmlAccessType.FIELD)
@Data
public class AttributedBoolean implements Serializable {
    @XmlValue
    private final Boolean value;
    @XmlAttribute(
            name = "type",
            namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
    )
    private final String attribute;

    public AttributedBoolean() {
        this.attribute = AttributeTypeEnum.BOOLEAN.type;
        this.value = false;
    }

    public AttributedBoolean(Boolean value) {
        this.attribute = AttributeTypeEnum.BOOLEAN.type;
        this.value = value;
    }
}