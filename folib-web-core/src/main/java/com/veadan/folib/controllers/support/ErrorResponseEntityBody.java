package com.veadan.folib.controllers.support;


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * @author veadan
 */
@XmlRootElement(name = "error")
@XmlAccessorType(XmlAccessType.NONE)
public class ErrorResponseEntityBody
{

    @XmlElement(name = "error")
    private String error;

    public ErrorResponseEntityBody()
    {
    }

    public ErrorResponseEntityBody(String error)
    {
        this.error = error;
    }

    public String getError()
    {
        return error;
    }
}
