package com.veadan.folib.controllers.support;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author Przemyslaw Fusik
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
