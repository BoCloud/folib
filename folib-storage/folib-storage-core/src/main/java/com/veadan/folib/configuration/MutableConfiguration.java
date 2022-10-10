package com.veadan.folib.configuration;

import com.beust.jcommander.internal.Lists;
import com.fasterxml.jackson.annotation.JsonRootName;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.VulnerabilitiesDto;
import com.veadan.folib.storage.routing.MutableRoutingRules;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * @author mtodorov
 * @author Veadan
 * @author veadan
 */
@JsonRootName("configuration")
public class MutableConfiguration
        implements Serializable {

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

    /**
     * 平台级别漏洞黑白名单
     */
    private VulnerabilitiesDto vulnerabilities = new VulnerabilitiesDto();

    private MutableRoutingRules routingRules = new MutableRoutingRules();

    private MutableCorsConfiguration corsConfiguration = new MutableCorsConfiguration();

    private MutableSmtpConfiguration smtpConfiguration = new MutableSmtpConfiguration();

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getInstanceName() {
        return instanceName;
    }

    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getRevision() {
        return revision;
    }

    public void setRevision(String revision) {
        this.revision = revision;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public void setBaseUrl(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public MutableProxyConfiguration getProxyConfiguration() {
        return proxyConfiguration;
    }

    public void setProxyConfiguration(MutableProxyConfiguration proxyConfiguration) {
        this.proxyConfiguration = proxyConfiguration;
    }

    public MutableSessionConfiguration getSessionConfiguration() {
        return sessionConfiguration;
    }

    public void setSessionConfiguration(MutableSessionConfiguration sessionConfiguration) {
        this.sessionConfiguration = sessionConfiguration;
    }

    public Map<String, StorageDto> getStorages() {
        return storages;
    }

    public void setStorages(Map<String, StorageDto> storages) {
        this.storages = storages;
    }

    public void addStorage(StorageDto storage) {
        String key = storage.getId();
        if (key == null || key.isEmpty()) {
            throw new IllegalArgumentException("Null keys are not supported!");
        }

        storages.put(key, storage);
    }

    public void upDateStorage(StorageDto storage) {


    }

    public void addStorageIfNotExist(StorageDto storage) {
        String key = storage.getId();
        if (key == null || key.isEmpty()) {
            throw new IllegalArgumentException("Null keys are not supported!");
        }

        storages.put(key, storage);
    }


    public StorageDto getStorage(String storageId) {
        return storages.get(storageId);
    }

    public void removeStorage(StorageDto storage) {
        storages.remove(storage.getId());
    }

    public MutableRoutingRules getRoutingRules() {
        return routingRules;
    }

    public void setRoutingRules(MutableRoutingRules routingRules) {
        this.routingRules = routingRules;
    }

    public MutableRemoteRepositoriesConfiguration getRemoteRepositoriesConfiguration() {
        return remoteRepositoriesConfiguration;
    }

    public void setRemoteRepositoriesConfiguration(MutableRemoteRepositoriesConfiguration remoteRepositoriesConfiguration) {
        this.remoteRepositoriesConfiguration = remoteRepositoriesConfiguration;
    }

    public MutableCorsConfiguration getCorsConfiguration() {
        return corsConfiguration;
    }

    public void setCorsConfiguration(final MutableCorsConfiguration corsConfiguration) {
        this.corsConfiguration = corsConfiguration;
    }

    public MutableSmtpConfiguration getSmtpConfiguration() {
        return smtpConfiguration;
    }

    public void setSmtpConfiguration(final MutableSmtpConfiguration smtpConfiguration) {
        this.smtpConfiguration = smtpConfiguration;
    }

    public VulnerabilitiesDto getVulnerabilities() {
        return vulnerabilities;
    }

    public void setVulnerabilities(VulnerabilitiesDto vulnerabilities) {
        this.vulnerabilities = vulnerabilities;
    }

    public void addVulnerabilitiesWhite(String white) {
        if (StringUtils.isBlank(white)) {
            return;
        }
        List<String> whiteList;
        if (StringUtils.isNotBlank(this.vulnerabilities.getWhite())) {
            whiteList = Lists.newArrayList(Arrays.asList(this.vulnerabilities.getWhite().split(",")));
        } else {
            whiteList = Lists.newArrayList();
        }
        List<String> addWhiteList = Lists.newArrayList(Arrays.asList(white.split(",")));
        addWhiteList.forEach(item -> {
            if (!whiteList.contains(item)) {
                whiteList.add(item);
            }
        });
        this.vulnerabilities.setWhite(String.join(",", whiteList));
    }

    public void addVulnerabilitiesBlack(String black) {
        if (StringUtils.isBlank(black)) {
            return;
        }
        List<String> blackList;
        if (StringUtils.isNotBlank(this.vulnerabilities.getBlack())) {
            blackList = Lists.newArrayList(Arrays.asList(this.vulnerabilities.getBlack().split(",")));
        } else {
            blackList = Lists.newArrayList();
        }
        List<String> addBlackList = Lists.newArrayList(Arrays.asList(black.split(",")));
        addBlackList.forEach(item -> {
            if (!blackList.contains(item)) {
                blackList.add(item);
            }
        });
        this.vulnerabilities.setBlack(String.join(",", blackList));
    }

    public void removeVulnerabilitiesWhite(String white) {
        if (StringUtils.isNotBlank(white) && StringUtils.isNotBlank(this.vulnerabilities.getWhite())) {
            List<String> whiteList = Lists.newArrayList(Arrays.asList(this.vulnerabilities.getWhite().split(",")));
            List<String> removeWhiteList = Lists.newArrayList(Arrays.asList(white.split(",")));
            removeWhiteList.forEach(item ->{
                whiteList.removeIf(whiteIterator -> whiteIterator.equals(item));
            });
            this.vulnerabilities.setWhite(String.join(",", whiteList));
        }
    }

    public void removeVulnerabilitiesBlack(String black) {
        if (StringUtils.isNotBlank(black) && StringUtils.isNotBlank(this.vulnerabilities.getBlack())) {
            List<String> blackList = Lists.newArrayList(Arrays.asList(this.vulnerabilities.getBlack().split(",")));
            List<String> removeBlackList = Lists.newArrayList(Arrays.asList(black.split(",")));
            removeBlackList.forEach(item ->{
                blackList.removeIf(whiteIterator -> whiteIterator.equals(item));
            });
            this.vulnerabilities.setWhite(String.join(",", blackList));
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
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
                Objects.equal(smtpConfiguration, that.smtpConfiguration) &&
                Objects.equal(vulnerabilities, that.vulnerabilities);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(version, baseUrl, port, proxyConfiguration, sessionConfiguration, storages,
                routingRules, vulnerabilities, remoteRepositoriesConfiguration, corsConfiguration, smtpConfiguration);
    }

    @Override
    public String toString() {
        return MoreObjects.toStringHelper(this)
                .add("\n\tinstanceName", instanceName)
                .add("\n\tversion", version)
                .add("\n\tbaseUrl", baseUrl)
                .add("\n\tport", port)
                .add("\n\tproxyConfiguration", proxyConfiguration)
                .add("\n\tsessionConfiguration", sessionConfiguration)
                .add("\n\tstorages", storages)
                .add("\n\troutingRules", routingRules)
                .add("\n\tvulnerabilities", vulnerabilities)
                .add("\n\tremoteRepositoriesConfiguration", remoteRepositoriesConfiguration)
                .add("\n\tcorsConfiguration", corsConfiguration)
                .add("\n\tsmtpConfiguration", smtpConfiguration)
                .toString();
    }


}
