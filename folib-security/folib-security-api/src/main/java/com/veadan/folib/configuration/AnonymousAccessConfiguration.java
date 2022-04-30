package com.veadan.folib.configuration;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * @author mtodorov
 */
@XmlRootElement(name = "anonymous-access")
@XmlAccessorType(XmlAccessType.FIELD)
public class AnonymousAccessConfiguration
{

    private boolean enabled = true;

    private String username = "anonymous";

    private String password = "password";

    public AnonymousAccessConfiguration()
    {
    }

    public boolean isEnabled()
    {
        return enabled;
    }

    public void setEnabled(boolean enabled)
    {
        this.enabled = enabled;
    }

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
