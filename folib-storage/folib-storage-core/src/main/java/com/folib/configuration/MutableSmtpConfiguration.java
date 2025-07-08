package com.folib.configuration;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Steve Todorov
 * @author Veadan
 */
public class MutableSmtpConfiguration
        implements Serializable
{

    private String host;

    private Integer port;

    private String connection;

    private String username;

    private String password;

    @JsonCreator
    public MutableSmtpConfiguration()
    {
    }

    @JsonCreator
    public MutableSmtpConfiguration(@JsonProperty("host") String host,
                                    @JsonProperty("port") Integer port,
                                    @JsonProperty("connection") String connection,
                                    @JsonProperty("username") String username,
                                    @JsonProperty("password") String password)
    {
        this.host = host;
        this.port = port;
        this.connection = connection;
        this.username = username;
        this.password = password;
    }

    public String getHost()
    {
        return host;
    }

    public void setHost(String host)
    {
        this.host = host;
    }

    public Integer getPort()
    {
        return port;
    }

    public void setPort(Integer port)
    {
        this.port = port;
    }

    public String getConnection()
    {
        return connection;
    }

    public void setConnection(String connection)
    {
        this.connection = connection;
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
