package com.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class InstanceNameEntityBody
{

    @JsonProperty("instanceName")
    private String instanceName;


    @JsonCreator
    public InstanceNameEntityBody(@JsonProperty("instanceName") String instanceName)
    {
        this.instanceName = instanceName;
    }

    public String getInstanceName()
    {
        return instanceName;
    }

    public void setInstanceName(String instanceName)
    {
        this.instanceName = instanceName;
    }

}
