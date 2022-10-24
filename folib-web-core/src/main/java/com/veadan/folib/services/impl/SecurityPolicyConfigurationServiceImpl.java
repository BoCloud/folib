package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.domain.Vulnerability;
import com.veadan.folib.forms.configuration.SecurityPolicyConfigurationForm;
import com.veadan.folib.repositories.VulnerabilityRepository;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.SecurityPolicyConfigurationService;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.transaction.Transactional;
import java.io.IOException;
import java.util.Optional;

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


    @Override
    public void setVulnerabilitiesWhites(String whites) throws IOException {
        configurationManagementService.setVulnerabilitiesWhites(whites);
    }

    @Override
    public void setVulnerabilitiesBlacks(String blacks) throws IOException {
        configurationManagementService.setVulnerabilitiesBlacks(blacks);
    }

    @Override
    public void addVulnerabilitiesWhite(String white) throws IOException {
        checkParams(white);
        configurationManagementService.addVulnerabilitiesWhite(white);
    }

    @Override
    public void addVulnerabilitiesBlack(String black) throws IOException {
        checkParams(black);
        configurationManagementService.addVulnerabilitiesBlack(black);
    }

    @Override
    public void removeVulnerabilitiesWhite(String white) throws IOException {
        checkParams(white);
        configurationManagementService.removeVulnerabilitiesWhite(white);
    }

    @Override
    public void removeVulnerabilitiesBlack(String black) throws IOException {
        checkParams(black);
        configurationManagementService.removeVulnerabilitiesBlack(black);
    }

    @Override
    public void saveOrUpdateNotify(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = MutableSecurityPolicyConfiguration.builder().levels(securityPolicyConfigurationForm.getLevels())
                .notifyScopes(securityPolicyConfigurationForm.getNotifyScopes()).receiverUsers(securityPolicyConfigurationForm.getReceiverUsers()).receiverEmails(securityPolicyConfigurationForm.getReceiverEmails()).build();
        configurationManagementService.saveOrUpdateNotify(mutableSecurityPolicyConfiguration);
    }

    @Override
    public void saveOrUpdateBlock(SecurityPolicyConfigurationForm securityPolicyConfigurationForm) throws IOException {
        MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = MutableSecurityPolicyConfiguration.builder().blockType(securityPolicyConfigurationForm.getBlockType())
                .blockLevels(securityPolicyConfigurationForm.getBlockLevels()).filterWhites(securityPolicyConfigurationForm.getFilterWhites()).build();
        configurationManagementService.saveOrUpdateBlock(mutableSecurityPolicyConfiguration);
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
}
