package com.folib.authentication.api;

import java.util.Map;

public class AuthenticationItem
{

    private String name;

    private Integer order;

    private Boolean enabled;

    private String type;

    public AuthenticationItem()
    {
    }

    public AuthenticationItem(String name,
                              String type)
    {
        this.name = name;
        this.type = type;
    }

    public AuthenticationItem(String name,
                              String type,
                              Map<String, Object> source)
    {
        this(name, type);

        this.order = (Integer) source.get("order");
        this.enabled = (Boolean) source.get("enabled");
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public Integer getOrder()
    {
        return order;
    }

    public void setOrder(Integer order)
    {
        this.order = order;
    }

    public Boolean getEnabled()
    {
        return enabled;
    }

    public void setEnabled(Boolean enabled)
    {
        this.enabled = enabled;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }

}
