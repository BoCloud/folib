package com.veadan.folib.controllers.login;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author veadan
 */
//@XmlRootElement(name = "login-input")
//@XmlAccessorType(XmlAccessType.NONE)
public class LoginInput
{

//    @XmlElement
    private String username;

//    @XmlElement
    private String password;

    public String getUsername()
    {
        return username;
    }

    public void setUsername(String username)
    {
        this.username = username;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }
}
