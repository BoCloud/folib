package com.veadan.folib.controllers.support;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Pablo Tirado
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class NumberOfConnectionsEntityBody
{

    @JsonProperty("numberOfConnections")
    private int numberOfConnections;

    @JsonCreator
    public NumberOfConnectionsEntityBody(@JsonProperty("numberOfConnections") int numberOfConnections)
    {
        this.numberOfConnections = numberOfConnections;
    }

    public int getNumberOfConnections()
    {
        return numberOfConnections;
    }

    public void setNumberOfConnections(int numberOfConnections)
    {
        this.numberOfConnections = numberOfConnections;
    }
}
