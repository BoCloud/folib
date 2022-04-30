package com.veadan.folib.configuration;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;

/**
 * @author mtodorov
 * @author Pablo Tirado
 */
public class MutableProxyConfiguration
        implements Serializable
{

    private String host;

    private Integer port;

    private String username;

    private String password;

    /**
     * Proxy type (HTTP, SOCKS5, etc)
     */
    private String type;

    private List<String> nonProxyHosts = new ArrayList<>();

    @JsonCreator
    public MutableProxyConfiguration()
    {
    }

    @JsonCreator
    public MutableProxyConfiguration(@JsonProperty("host") String host,
                                     @JsonProperty("port") Integer port,
                                     @JsonProperty("username") String username,
                                     @JsonProperty("password") String password,
                                     @JsonProperty("type") String type,
                                     @JsonProperty("nonProxyHosts") List<String> nonProxyHosts)
    {
        this.host = host;
        this.port = port;
        this.username = username;
        this.password = password;
        this.type = type;
        this.nonProxyHosts = nonProxyHosts;
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

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }

    public List<String> getNonProxyHosts()
    {
        return nonProxyHosts;
    }

    public void setNonProxyHosts(List<String> nonProxyHosts)
    {
        this.nonProxyHosts = nonProxyHosts;
    }

    public void addNonProxyHost(String host)
    {
        nonProxyHosts.add(host);
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MutableProxyConfiguration that = (MutableProxyConfiguration) o;
        return Objects.equal(port, that.port) &&
               Objects.equal(host, that.host) &&
               Objects.equal(username, that.username) &&
               Objects.equal(password, that.password) &&
               Objects.equal(type, that.type) &&
               Objects.equal(nonProxyHosts, that.nonProxyHosts);
    }

    @Override
    public int hashCode()
    {
        return Objects.hashCode(host, port, username, password, type, nonProxyHosts);
    }

    @Override
    public String toString()
    {
        return MoreObjects.toStringHelper(this)
                          .add("host", host)
                          .add("port", port)
                          .add("username", username)
                          .add("password", password)
                          .add("type", type)
                          .add("nonProxyHosts", nonProxyHosts)
                          .toString();
    }

}
