package com.veadan.folib.services.impl;

import com.google.common.collect.Sets;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.enums.BlockTypeEnum;
import com.veadan.folib.forms.configuration.SecurityPolicyConfigurationForm;
import com.veadan.folib.repositories.VulnerabilityRepository;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.SecurityPolicyConfigurationService;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.util.Optional;
import java.util.Set;

/**
 * @author leipenghui
 * @date 2022/10/21
 **/
@Service
@Transactional
public class SecurityPolicyConfigurationServiceImpl implements SecurityPolicyConfigurationService {

    @Inject
    private VulnerabilityRepository vulnerabilityRepository;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private ClusterSyncService clusterSyncService;


    @Override
    public void setVulnerabilitiesWhites(String whites) throws IOException {
        configurationManagementService.setVulnerabilitiesWhites(whites);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void setVulnerabilitiesBlacks(String blacks) throws IOException {
        configurationManagementService.setVulnerabilitiesBlacks(blacks);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void addVulnerabilitiesWhite(String white) throws IOException {
        checkParams(white);
        configurationManagementService.addVulnerabilitiesWhite(white);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void addVulnerabilitiesBlack(String black) throws IOException {
        checkParams(black);
        configurationManagementService.addVulnerabilitiesBlack(black);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void removeVulnerabilitiesWhite(String white) throws IOException {
        checkParams(white);
        configurationManagementService.removeVulnerabilitiesWhite(white);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void removeVulnerabilitiesBlack(String black) throws IOException {
        checkParams(black);
        configurationManagementService.removeVulnerabilitiesBlack(black);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void saveOrUpdateNotify(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = MutableSecurityPolicyConfiguration.builder().levels(securityPolicyConfigurationForm.getLevels())
                .notifyScopes(securityPolicyConfigurationForm.getNotifyScopes()).receiverUsers(securityPolicyConfigurationForm.getReceiverUsers()).receiverEmails(securityPolicyConfigurationForm.getReceiverEmails()).build();
        configurationManagementService.saveOrUpdateNotify(mutableSecurityPolicyConfiguration);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void saveOrUpdateBlock(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        MutableSecurityPolicyConfiguration oldMutableSecurityPolicyConfiguration = configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration();
        MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = MutableSecurityPolicyConfiguration.builder().blockType(securityPolicyConfigurationForm.getBlockType())
                .blockLevels(securityPolicyConfigurationForm.getBlockLevels()).filterWhites(securityPolicyConfigurationForm.getFilterWhites()).packageNames(oldMutableSecurityPolicyConfiguration.getPackageNames()).build();
        configurationManagementService.saveOrUpdateBlock(mutableSecurityPolicyConfiguration);
        syncDataSecurityPolicyConfiguration();
    }

    @Override
    public void addPackageName(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        if (CollectionUtils.isNotEmpty(securityPolicyConfigurationForm.getPackageNames())) {
            MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration();
            Set<String> packageNames = mutableSecurityPolicyConfiguration.getPackageNames();
            if (CollectionUtils.isEmpty(packageNames)) {
                packageNames = Sets.newLinkedHashSet();
            }
            packageNames.addAll(securityPolicyConfigurationForm.getPackageNames());
            mutableSecurityPolicyConfiguration.setPackageNames(packageNames);
            mutableSecurityPolicyConfiguration.setBlockType(BlockTypeEnum.PACKAGE_NAME.getType());
            configurationManagementService.saveOrUpdateBlock(mutableSecurityPolicyConfiguration);
            syncDataSecurityPolicyConfiguration();
        }
    }

    @Override
    public void deletePackageName(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        if (CollectionUtils.isNotEmpty(securityPolicyConfigurationForm.getPackageNames())) {
            MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration();
            Set<String> packageNames = mutableSecurityPolicyConfiguration.getPackageNames();
            if (CollectionUtils.isNotEmpty(packageNames)) {
                packageNames.removeAll(securityPolicyConfigurationForm.getPackageNames());
                mutableSecurityPolicyConfiguration.setBlockType(BlockTypeEnum.PACKAGE_NAME.getType());
                configurationManagementService.saveOrUpdateBlock(mutableSecurityPolicyConfiguration);
                syncDataSecurityPolicyConfiguration();
            }
        }
    }

    @Override
    public SecurityPolicyConfigurationForm config() {
        return SecurityPolicyConfigurationForm.fromConfiguration(configurationManagementService.getConfiguration().getSecurityPolicyConfiguration());
    }

    private void checkParams(String uuid) {
        Optional<Vulnerability> vulnerabilityOptional = vulnerabilityRepository.findById(uuid);
        if (!vulnerabilityOptional.isPresent()) {
            throw new RuntimeException(uuid + "漏洞编号不存在！");
        }
    }

    /**
     * 向其他集群节点同步安全策略配置
     */
    private void syncDataSecurityPolicyConfiguration() {
        clusterSyncService.syncSecurityPolicyConfiguration(configurationManagementService.getMutableConfigurationClone().getSecurityPolicyConfiguration());
    }
}
