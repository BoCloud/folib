package com.folib.configuration;

import javax.annotation.concurrent.Immutable;

import java.util.Collections;
import java.util.List;

import com.google.common.collect.ImmutableList;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

/**
 * @author veadan
 * @see MutableProxyConfiguration
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class ProxyConfiguration
{

    private String host;

    private Integer port;

    private String username;

    private String password;

    private String type;

    private List<String> nonProxyHosts;

    ProxyConfiguration()
    {

    }

    public ProxyConfiguration(final MutableProxyConfiguration delegate)
    {
        this.host = delegate.getHost();
        this.port = delegate.getPort();
        this.username = delegate.getUsername();
        this.password = delegate.getPassword();
        this.type = delegate.getType();
        this.nonProxyHosts = immuteNonProxyHosts(delegate.getNonProxyHosts());
    }

    private List<String> immuteNonProxyHosts(final List<String> source)
    {
        return source != null ? ImmutableList.copyOf(source) : Collections.emptyList();
    }

    public String getHost()
    {
        return host;
    }

    public Integer getPort()
    {
        return port;
    }

    public String getUsername()
    {
        return username;
    }

    public String getPassword()
    {
        return password;
    }

    public String getType()
    {
        return type;
    }

    public List<String> getNonProxyHosts()
    {
        return nonProxyHosts;
    }
}
