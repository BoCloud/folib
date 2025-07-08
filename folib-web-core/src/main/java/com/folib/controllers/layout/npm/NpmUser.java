package com.folib.controllers.layout.npm;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSetter;

@JsonIgnoreProperties(ignoreUnknown = true)
public class NpmUser
{

    private String name;

    private String password;

    @JsonCreator
    public NpmUser()
    {
    }

    @JsonCreator
    public NpmUser(@JsonProperty(value = "name", required = true) String name,
                   @JsonProperty(value = "password", required = true) String password)
    {
        this.name = name;
        this.password = password;
    }

    public String getName()
    {
        return name;
    }

    public String getPassword()
    {
        return password;
    }
}
