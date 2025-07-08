package com.folib.providers.layout.p2;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;

import java.util.ArrayList;
import java.util.List;

public class P2Mappings
{

    private Integer size;

    private List<P2Rule> rules = new ArrayList<>();

    @XmlAttribute
    public Integer getSize()
    {
        return size;
    }

    public void setSize(Integer size)
    {
        this.size = size;
    }

    @XmlElement(name = "rule")
    public List<P2Rule> getRules()
    {
        return rules;
    }

    public void setRules(List<P2Rule> rules)
    {
        this.rules = rules;
    }

}
