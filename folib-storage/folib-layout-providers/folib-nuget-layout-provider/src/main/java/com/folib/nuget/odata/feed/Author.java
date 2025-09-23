package com.folib.nuget.odata.feed;

import com.google.common.collect.Lists;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import lombok.Data;
import lombok.NoArgsConstructor;


import java.io.Serializable;
import java.util.List;

@XmlType(
    name = "author"
)
@Data
@NoArgsConstructor
public class Author implements Serializable {
    @XmlElement(
            name = "name"
    )
    private List<String> names;

    @XmlTransient
    public List<String> getNames() {
        return names;
    }

    public Author(String authors) {
        this.names = Lists.newArrayList(new String[]{authors});
    }
}
