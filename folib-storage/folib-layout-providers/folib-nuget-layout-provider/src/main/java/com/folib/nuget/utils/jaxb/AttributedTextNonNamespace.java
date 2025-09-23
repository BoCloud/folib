package com.folib.nuget.utils.jaxb;


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;
import lombok.Data;


import java.io.Serializable;




@XmlAccessorType(XmlAccessType.FIELD)
@Data
public class AttributedTextNonNamespace implements Serializable {
    @XmlValue
    private final String value;
    @XmlAttribute
    private final String type;

    public AttributedTextNonNamespace() {
        this.type = AttributeTypeEnum.TEXT.type;
        this.value = null;
    }

    public AttributedTextNonNamespace(String value) {
        this.type = AttributeTypeEnum.TEXT.type;
        this.value = value;
    }
}
