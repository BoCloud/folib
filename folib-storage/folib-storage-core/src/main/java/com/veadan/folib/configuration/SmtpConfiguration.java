package com.veadan.folib.configuration;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;

import javax.annotation.concurrent.Immutable;

@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
public class SmtpConfiguration
{

    @XmlAttribute
    private String host;

    @XmlAttribute
    private Integer port;

    @XmlAttribute
    private String connection;

    @XmlAttribute
    private String username;

    @XmlAttribute
    private String password;

    SmtpConfiguration()
    {
    }

    public SmtpConfiguration(final MutableSmtpConfiguration source)
    {
        this.host = source.getHost();
        this.port = source.getPort();
        this.connection = source.getConnection();
        this.username = source.getUsername();
        this.password = source.getPassword();
    }

    public String getHost()
    {
        return host;
    }

    public Integer getPort()
    {
        return port;
    }

    public String getConnection()
    {
        return connection;
    }

    public String getUsername()
    {
        return username;
    }

    public String getPassword()
    {
        return password;
    }
}
