package com.folib.nuget.utils.jaxb;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;
import lombok.Data;

import java.io.Serializable;


@XmlAccessorType(XmlAccessType.FIELD)
@Data
public class AttributedInt64 implements Serializable {
    @XmlValue
    private final Long value;
    @XmlAttribute(
            name = "type",
            namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
    )
    private final String attribute;

    public AttributedInt64() {
        this.attribute = AttributeTypeEnum.INT_64.type;
        this.value = 0L;
    }

    public AttributedInt64(Long value) {
        this.attribute = AttributeTypeEnum.INT_64.type;
        this.value = value;
    }
}
