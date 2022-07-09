package com.veadan.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class PortEntityBody
{

    @JsonProperty("port")
    private int port;

    @JsonCreator
    public PortEntityBody(@JsonProperty("port") int port)
    {
        this.port = port;
    }

    public int getPort()
    {
        return port;
    }

    public void setPort(int port)
    {
        this.port = port;
    }
}
