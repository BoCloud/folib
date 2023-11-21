package com.veadan.folib.configuration;

import com.beust.jcommander.internal.Lists;
import com.fasterxml.jackson.annotation.JsonRootName;
import com.google.common.base.MoreObjects;
import com.google.common.base.Objects;
import com.google.common.collect.Sets;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.routing.MutableRoutingRules;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.*;

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
     * 节点传输速率（KB/s）
     */
    private Integer kbps;
    /**
     * 节点传输切片大小（MB）
     */
    private Long sliceMbSize = 50L;

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
     * k: clusterDispatch enname
     * v: clusterDispatchNode
     */
    private Map<String, ClusterDispatchNodeDto> clusterDispatchNode = new LinkedHashMap<>();

    /**
     * 安全策略配置
     */
    private MutableSecurityPolicyConfiguration securityPolicyConfiguration = new MutableSecurityPolicyConfiguration();

    private MutableRoutingRules routingRules = new MutableRoutingRules();

    private MutableCorsConfiguration corsConfiguration = new MutableCorsConfiguration();

    private MutableSmtpConfiguration smtpConfiguration = new MutableSmtpConfiguration();
    /**
     * 全局元数据配置
     */
    private Map<String, MutableMetadataConfiguration> metadataConfiguration = new LinkedHashMap<>();

    /**
     * webhook配置
     */
    private Map<String, MutableWebhookConfiguration> webhookConfiguration = new LinkedHashMap<>();

    private MutableAdvancedConfiguration advancedConfiguration = new MutableAdvancedConfiguration();

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

    public Integer getKbps() {
        return kbps;
    }

    public void setKbps(Integer kbps) {
        this.kbps = kbps;
    }

    public Long getSliceMbSize() {
        return sliceMbSize;
    }

    public void setSliceMbSize(Long sliceMbSize) {
        this.sliceMbSize = sliceMbSize;
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

    public Map<String, ClusterDispatchNodeDto> getClusterDispatchNode() {
        return clusterDispatchNode;
    }

    public void setClusterDispatchNode(Map<String, ClusterDispatchNodeDto> clusterDispatchNode) {
        this.clusterDispatchNode = clusterDispatchNode;
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

    public void addClusterDispatchNode(ClusterDispatchNodeDto clusterDispatchNodeDto) {
        String key = clusterDispatchNodeDto.getClusterEnName();
        if (key == null || key.isEmpty()) {
            throw new IllegalArgumentException("Null keys are not supported!");
        }
        clusterDispatchNode.put(key, clusterDispatchNodeDto);
    }

    public Map<String, MutableMetadataConfiguration> getMetadataConfiguration() {
        return metadataConfiguration;
    }

    public void setMetadataConfiguration(Map<String, MutableMetadataConfiguration> metadataConfiguration) {
        this.metadataConfiguration = metadataConfiguration;
    }

    public void addOrUpdateMetadataConfiguration(MutableMetadataConfiguration mutableMetadataConfiguration) {
        String key = mutableMetadataConfiguration.getKey();
        if (key == null || key.isEmpty()) {
            throw new IllegalArgumentException("Null keys are not supported!");
        }
        this.metadataConfiguration.put(key, mutableMetadataConfiguration);
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

    public MutableSecurityPolicyConfiguration getSecurityPolicyConfiguration() {
        return securityPolicyConfiguration;
    }

    public void setSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration securityPolicyConfiguration) {
        this.securityPolicyConfiguration = securityPolicyConfiguration;
    }

    public void saveOrUpdateNotify(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) {
        this.securityPolicyConfiguration.setLevels(mutableSecurityPolicyConfiguration.getLevels());
        this.securityPolicyConfiguration.setNotifyScopes(mutableSecurityPolicyConfiguration.getNotifyScopes());
        this.securityPolicyConfiguration.setReceiverUsers(mutableSecurityPolicyConfiguration.getReceiverUsers());
        this.securityPolicyConfiguration.setReceiverEmails(mutableSecurityPolicyConfiguration.getReceiverEmails());
    }

    public void saveOrUpdateBlock(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) {
        this.securityPolicyConfiguration.setBlockType(mutableSecurityPolicyConfiguration.getBlockType());
        this.securityPolicyConfiguration.setBlockLevels(mutableSecurityPolicyConfiguration.getBlockLevels());
        this.securityPolicyConfiguration.setFilterWhites(mutableSecurityPolicyConfiguration.getFilterWhites());
        this.securityPolicyConfiguration.setPackageNames(mutableSecurityPolicyConfiguration.getPackageNames());
    }

    public void setVulnerabilityBlacks(Set<String> blacks) {
        this.securityPolicyConfiguration.setBlacks(blacks);
    }

    public void setVulnerabilityWhites(Set<String> whites) {
        this.securityPolicyConfiguration.setWhites(whites);
    }

    public void addVulnerabilitiesWhite(String white) {
        if (StringUtils.isBlank(white)) {
            return;
        }
        List<String> addWhites = Lists.newArrayList(Arrays.asList(white.split(",")));
        addWhites.forEach(item -> {
            if (this.securityPolicyConfiguration.getWhites().contains(item)) {
                throw new RuntimeException(item + "已在白名单中");
            }
            if (this.securityPolicyConfiguration.getBlacks().contains(item)) {
                throw new RuntimeException(item + "已在黑名单中");
            }
            this.securityPolicyConfiguration.addWhite(item);
        });
    }

    public void addVulnerabilitiesBlack(String black) {
        if (StringUtils.isBlank(black)) {
            return;
        }
        List<String> addBlacks = Lists.newArrayList(Arrays.asList(black.split(",")));
        addBlacks.forEach(item -> {
            if (this.securityPolicyConfiguration.getWhites().contains(item)) {
                throw new RuntimeException(item + "已在白名单中");
            }
            if (this.securityPolicyConfiguration.getBlacks().contains(item)) {
                throw new RuntimeException(item + "已在黑名单中");
            }
            this.securityPolicyConfiguration.addBlack(item);
        });
    }

    public void removeVulnerabilitiesWhite(String white) {
        Set<String> whites = getSecurityPolicyConfiguration().getWhites();
        if (StringUtils.isNotBlank(white) && CollectionUtils.isNotEmpty(whites)) {
            Set<String> removeWhites = Sets.newHashSet(Arrays.asList(white.split(",")));
            whites.removeAll(removeWhites);
            this.securityPolicyConfiguration.setWhites(whites);
        }
    }

    public void removeVulnerabilitiesBlack(String black) {
        Set<String> blacks = getSecurityPolicyConfiguration().getBlacks();
        if (StringUtils.isNotBlank(black) && CollectionUtils.isNotEmpty(blacks)) {
            Set<String> removeBlacks = Sets.newHashSet(Arrays.asList(black.split(",")));
            blacks.removeAll(removeBlacks);
            this.securityPolicyConfiguration.setBlacks(blacks);
        }
    }

    public Map<String, MutableWebhookConfiguration> getWebhookConfiguration() {
        return webhookConfiguration;
    }

    public void setWebhookConfiguration(Map<String, MutableWebhookConfiguration> webhookConfiguration) {
        this.webhookConfiguration = webhookConfiguration;
    }

    public void addOrUpdateWebhookConfiguration(MutableWebhookConfiguration mutableWebhookConfiguration) {
        String uuid = mutableWebhookConfiguration.getUuid();
        if (StringUtils.isBlank(uuid)) {
            throw new IllegalArgumentException("Null keys are not supported!");
        }
        this.webhookConfiguration.put(uuid, mutableWebhookConfiguration);
    }

    public MutableAdvancedConfiguration getAdvancedConfiguration() {
        return advancedConfiguration;
    }

    public void setAdvancedConfiguration(MutableAdvancedConfiguration advancedConfiguration) {
        this.advancedConfiguration = advancedConfiguration;
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
                Objects.equal(securityPolicyConfiguration, that.securityPolicyConfiguration) &&
                Objects.equal(metadataConfiguration, that.metadataConfiguration) &&
                Objects.equal(webhookConfiguration, that.webhookConfiguration) &&
                Objects.equal(advancedConfiguration, that.advancedConfiguration);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(version, baseUrl, port, proxyConfiguration, sessionConfiguration, storages,
                routingRules, securityPolicyConfiguration, remoteRepositoriesConfiguration, corsConfiguration, smtpConfiguration, metadataConfiguration, webhookConfiguration, advancedConfiguration);
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
                .add("\n\tsecurityPolicyConfiguration", securityPolicyConfiguration)
                .add("\n\tremoteRepositoriesConfiguration", remoteRepositoriesConfiguration)
                .add("\n\tcorsConfiguration", corsConfiguration)
                .add("\n\tsmtpConfiguration", smtpConfiguration)
                .add("\n\tmetadataConfiguration", metadataConfiguration)
                .add("\n\twebhookConfiguration", webhookConfiguration)
                .add("\n\tadvancedConfiguration", advancedConfiguration)
                .toString();
    }


}
