package com.veadan.folib.components.common;

import com.google.common.collect.Lists;
import com.veadan.folib.authentication.api.ldap.LdapAuthenticationConfigurationManager;
import com.veadan.folib.authentication.api.ldap.LdapConfiguration;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.forms.configuration.ServerSettingsForm;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.users.domain.Privileges;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.WebTarget;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class CommonComponent {

    @Inject
    @Lazy
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private LdapAuthenticationConfigurationManager ldapAuthenticationManager;

    /**
     * Client WebTarget 构建认证信息
     *
     * @param webTarget webTarget
     * @param username  username
     * @param password  password
     */
    public void authentication(WebTarget webTarget, String username, String password) {
        final HttpAuthenticationFeature authenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? HttpAuthenticationFeature.basic(username, password) : null;
        if (authenticationFeature != null) {
            webTarget.register(authenticationFeature);
            webTarget.property(ApacheClientProperties.REQUEST_CONFIG,
                    RequestConfig.custom().setCircularRedirectsAllowed(true).build());
        }
    }

    /**
     * 更新全局配置
     *
     * @param serverSettingsForm 全局配置
     * @throws Exception 异常
     */
    public void updateServerSettings(ServerSettingsForm serverSettingsForm) throws Exception {
        configurationManagementService.setBaseUrl(serverSettingsForm.getBaseUrl());
        configurationManagementService.setPort(serverSettingsForm.getPort());
        configurationManagementService.setKbps(serverSettingsForm.getKbps());
        configurationManagementService.setSliceMbSize(serverSettingsForm.getSliceMbSize());
        configurationManagementService.setInstanceName(serverSettingsForm.getInstanceName());
        if (serverSettingsForm.getCorsConfigurationForm() != null) {
            configurationManagementService.setCorsAllowedOrigins(
                    serverSettingsForm.getCorsConfigurationForm().getAllowedOrigins()
            );
        }
        if (serverSettingsForm.getSmtpConfigurationForm() != null) {
            // SMTP settings
            configurationManagementService.setSmtpSettings(
                    serverSettingsForm.getSmtpConfigurationForm().getMutableSmtpConfiguration()
            );
        }
        if (serverSettingsForm.getProxyConfigurationForm() != null) {
            // Global Proxy settings
            configurationManagementService.setProxyConfiguration(
                    null, null, serverSettingsForm.getProxyConfigurationForm().getMutableProxyConfiguration()
            );
        }
        if (serverSettingsForm.getAdvancedConfigurationForm() != null) {
            configurationManagementService.setAdvancedConfiguration(serverSettingsForm.getAdvancedConfigurationForm().getMutableProxyConfiguration());
            if (Boolean.FALSE.equals(serverSettingsForm.getAdvancedConfigurationForm().getAllowAnonymous())) {
                authorizationConfigService.clearPrivilegesAnonymous();
            } else if (Boolean.TRUE.equals(serverSettingsForm.getAdvancedConfigurationForm().getAllowAnonymous())) {
                authorizationConfigService.addPrivilegesToAnonymous(Lists.newArrayList(Privileges.ARTIFACTS_RESOLVE, Privileges.SEARCH_ARTIFACTS, Privileges.ARTIFACTS_VIEW));
            }
        }
    }

    /**
     * 更新Ldap配置
     *
     * @param ldapConfiguration Ldap配置
     * @throws Exception 异常
     */
    public void updateLdap(LdapConfiguration ldapConfiguration) throws Exception {
        ldapAuthenticationManager.updateConfiguration(ldapConfiguration);
    }

}
