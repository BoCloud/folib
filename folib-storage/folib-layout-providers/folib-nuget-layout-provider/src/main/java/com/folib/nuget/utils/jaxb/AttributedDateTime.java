package com.folib.nuget.utils.jaxb;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlValue;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;


import java.io.Serializable;
import java.time.LocalDateTime;



@Slf4j
@XmlAccessorType(XmlAccessType.FIELD)
@Data
public class AttributedDateTime implements Serializable {
    public static final String DATE_TIME_FORMAT_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ";
    @XmlValue
    private final String value;
    @XmlAttribute(
            name = "type",
            namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
    )
    private final String type;

    public AttributedDateTime() {
        this.type = AttributeTypeEnum.DATE_TIME.type;
        this.value = null;
    }

    public AttributedDateTime(String value) {
        this.type = AttributeTypeEnum.DATE_TIME.type;
        this.value = value;
    }

    public LocalDateTime getDateTime() {
        LocalDateTime dateTime = null;
        if (StringUtils.isNotBlank(this.value)) {
            try {
                dateTime = LocalDateTime.parse(this.value);
            } catch (Exception e) {
                log.error("Couldn't parse DateTime: {}", e.getMessage());
                log.debug("Couldn't parse DateTime", e);
            }
        }

        return dateTime;
    }
}
