package com.veadan.folib.controllers.environment;

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

/**
 * @author Veadan
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
        "name",
        "value",
})
public class EnvironmentInfo
        implements Comparable<EnvironmentInfo>
{

    @JsonProperty("name")
    private String name;

    @JsonProperty("value")
    private String value;

    @JsonCreator
    public EnvironmentInfo(@JsonProperty("name") String name,
                           @JsonProperty("value") String value)
    {
        this.name = name;
        this.value = value;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public String getValue()
    {
        return value;
    }

    public void setValue(String value)
    {
        this.value = value;
    }

    @Override
    public int compareTo(EnvironmentInfo other)
    {
        return getName().compareTo(other.getName());
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EnvironmentInfo that = (EnvironmentInfo) o;
        return Objects.equals(name, that.name);
    }

    @Override
    public int hashCode()
    {

        return Objects.hash(name);
    }
}
