package com.veadan.folib.configuration;

import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.routing.MutableRoutingRules;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonRootName;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;

/**
 * @author mtodorov
 * @author Pablo Tirado
 * @author veadan
 */
@JsonRootName("configuration")
public class MutableConfiguration
        implements Serializable
{

    private String id;

    private String instanceName = "folib";

    private String version = "1.0";

    private String revision;

    private String baseUrl = "http://localhost/";

    private int port = 48080;

    /**
     * The global proxy settings to use when no per-repository proxy settings have been defined.
     */
    private MutableProxyConfiguration proxyConfiguration;

    private MutableSessionConfiguration sessionConfiguration;

    private MutableRemoteRepositoriesConfiguration remoteRepositoriesConfiguration = MutableRemoteRepositoriesConfiguration.DEFAULT;

    /**
     * K: storageId
     * V: storage
     */
    private Map<String, StorageDto> storages = new LinkedHashMap<>();

    private MutableRoutingRules routingRules = new MutableRoutingRules();

    private MutableCorsConfiguration corsConfiguration = new MutableCorsConfiguration();

    private MutableSmtpConfiguration smtpConfiguration = new MutableSmtpConfiguration();

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public String getInstanceName()
    {
        return instanceName;
    }

    public void setInstanceName(String instanceName)
    {
        this.instanceName = instanceName;
    }

    public String getVersion()
    {
        return version;
    }

    public void setVersion(String version)
    {
        this.version = version;
    }

    public String getRevision()
    {
        return revision;
    }

    public void setRevision(String revision)
    {
        this.revision = revision;
    }

    public String getBaseUrl()
    {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl)
    {
        this.baseUrl = baseUrl;
    }

    public int getPort()
    {
        return port;
    }

    public void setPort(int port)
    {
        this.port = port;
    }

    public MutableProxyConfiguration getProxyConfiguration()
    {
        return proxyConfiguration;
    }

    public void setProxyConfiguration(MutableProxyConfiguration proxyConfiguration)
    {
        this.proxyConfiguration = proxyConfiguration;
    }

    public MutableSessionConfiguration getSessionConfiguration()
    {
        return sessionConfiguration;
    }

    public void setSessionConfiguration(MutableSessionConfiguration sessionConfiguration)
    {
        this.sessionConfiguration = sessionConfiguration;
    }

    public Map<String, StorageDto> getStorages()
    {
        return storages;
    }

    public void setStorages(Map<String, StorageDto> storages)
    {
        this.storages = storages;
    }

    public void addStorage(StorageDto storage)
    {
        String key = storage.getId();
        if (key == null || key.isEmpty())
        {
            throw new IllegalArgumentException("Null keys are not supported!");
        }

        storages.put(key, storage);
    }
    
    public void addStorageIfNotExist(StorageDto storage)
    {
        String key = storage.getId();
        if (key == null || key.isEmpty())
        {
            throw new IllegalArgumentException("Null keys are not supported!");
        }

        storages.put(key, storage);
    }


    public StorageDto getStorage(String storageId)
    {
        return storages.get(storageId);
    }

    public void removeStorage(StorageDto storage)
    {
        storages.remove(storage.getId());
    }

    public MutableRoutingRules getRoutingRules()
    {
        return routingRules;
    }

    public void setRoutingRules(MutableRoutingRules routingRules)
    {
        this.routingRules = routingRules;
    }

    public MutableRemoteRepositoriesConfiguration getRemoteRepositoriesConfiguration()
    {
        return remoteRepositoriesConfiguration;
    }

    public void setRemoteRepositoriesConfiguration(MutableRemoteRepositoriesConfiguration remoteRepositoriesConfiguration)
    {
        this.remoteRepositoriesConfiguration = remoteRepositoriesConfiguration;
    }

    public MutableCorsConfiguration getCorsConfiguration()
    {
        return corsConfiguration;
    }

    public void setCorsConfiguration(final MutableCorsConfiguration corsConfiguration)
    {
        this.corsConfiguration = corsConfiguration;
    }

    public MutableSmtpConfiguration getSmtpConfiguration()
    {
        return smtpConfiguration;
    }

    public void setSmtpConfiguration(final MutableSmtpConfiguration smtpConfiguration)
    {
        this.smtpConfiguration = smtpConfiguration;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        MutableConfiguration that = (MutableConfiguration) o;
        return port == that.port &&
               Objects.equal(instanceName, that.instanceName) &&
               Objects.equal(version, that.version) &&
               Objects.equal(baseUrl, that.baseUrl) &&
               Objects.equal(proxyConfiguration, that.proxyConfiguration) &&
               Objects.equal(sessionConfiguration, that.sessionConfiguration) &&
               Objects.equal(storages, that.storages) &&
               Objects.equal(routingRules, that.routingRules) &&
               Objects.equal(remoteRepositoriesConfiguration, that.remoteRepositoriesConfiguration) &&
               Objects.equal(corsConfiguration, that.corsConfiguration) &&
               Objects.equal(smtpConfiguration, that.smtpConfiguration);
    }

    @Override
    public int hashCode()
    {
        return Objects.hashCode(version, baseUrl, port, proxyConfiguration, sessionConfiguration, storages,
                                routingRules, remoteRepositoriesConfiguration, corsConfiguration, smtpConfiguration);
    }

    @Override
    public String toString()
    {
        return MoreObjects.toStringHelper(this)
                          .add("\n\tinstanceName", instanceName)
                          .add("\n\tversion", version)
                          .add("\n\tbaseUrl", baseUrl)
                          .add("\n\tport", port)
                          .add("\n\tproxyConfiguration", proxyConfiguration)
                          .add("\n\tsessionConfiguration", sessionConfiguration)
                          .add("\n\tstorages", storages)
                          .add("\n\troutingRules", routingRules)
                          .add("\n\tremoteRepositoriesConfiguration", remoteRepositoriesConfiguration)
                          .add("\n\tcorsConfiguration", corsConfiguration)
                          .add("\n\tsmtpConfiguration", smtpConfiguration)
                          .toString();
    }

}
