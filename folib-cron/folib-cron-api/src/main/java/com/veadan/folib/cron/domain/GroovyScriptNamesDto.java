package com.veadan.folib.cron.domain;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Yougeshwar
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class GroovyScriptNamesDto
{

    @XmlElement
    private List<String> list;

    public GroovyScriptNamesDto()
    {
        list = new ArrayList<>();
    }

    public void addName(String name)
    {
        list.add(name);
    }

    public List<String> getList()
    {
        return list;
    }

    public void setList(List<String> list)
    {
        this.list = list;
    }
}
