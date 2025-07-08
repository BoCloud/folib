package com.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Steve Todorov
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ExampleEntityBody
{

    @JsonProperty("name")
    private String name;

    @JsonCreator
    public ExampleEntityBody(@JsonProperty("name") String name)
    {
        this.name = name;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }
}
