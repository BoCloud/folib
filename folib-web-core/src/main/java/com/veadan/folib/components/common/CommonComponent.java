package com.veadan.folib.components.common;

import com.google.common.collect.Lists;
import com.veadan.folib.authentication.api.ldap.LdapAuthenticationConfigurationManager;
import com.veadan.folib.authentication.api.ldap.LdapConfiguration;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.cluster.FolibLockProperties;
import com.veadan.folib.cluster.SyncAuthorizationEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.cluster.dto.SyncAuthorizationDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.forms.configuration.ServerSettingsForm;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.AnonymousAuthenticationFilter;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.WebTarget;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

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

    @Inject
    @Lazy
    private StorageManagementService storageManagementService;

    @Inject
    @Lazy
    private ClusterSyncService clusterSyncService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    private FolibLockProperties folibLockProperties;
    @Inject
    private AuthoritiesProvider authoritiesProvider;
    @Inject
    private AnonymousAuthenticationFilter anonymousAuthenticationFilter;
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
                updateAnonymous();
            } else if (Boolean.TRUE.equals(serverSettingsForm.getAdvancedConfigurationForm().getAllowAnonymous())) {
                authorizationConfigService.addPrivilegesToAnonymous(Lists.newArrayList(Privileges.ARTIFACTS_RESOLVE, Privileges.SEARCH_ARTIFACTS, Privileges.ARTIFACTS_VIEW, Privileges.CONFIGURATION_VIEW_METADATA_CONFIGURATION));
                updateAnonymous();
            }
        }
    }

    private void updateAnonymous() {
        List<GrantedAuthority> authorities = AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS");
        Role role = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
        authorities.addAll(role.getAccessModel().getApiAuthorities());
        anonymousAuthenticationFilter.getAuthorities().clear();
        anonymousAuthenticationFilter.getAuthorities().addAll(authorities);
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

    public void handleStorageProvider() throws IOException {
        for (Map.Entry<String, StorageDto> entry : configurationManagementService.getMutableConfigurationClone().getStorages().entrySet()) {
            StorageDto storage = entry.getValue();
            storageManagementService.handleStorageProvider(storage);
            // 向其他集群节点同步storage
            SyncStorageDto syncStorageDto = new SyncStorageDto(storage, storage.getId(), SyncStorageEnum.UPDATE);
            clusterSyncService.syncStorage(syncStorageDto);
        }
    }

    public boolean isRepositoryResolvable(Repository repository) {
        final boolean isInService = repository.isInService();
        if (!isInService) {
            log.info("- Repository [{}] is not in service, skipping...",
                    repository.getStorageIdAndRepositoryId());
            return false;
        }
        return true;
    }

    public boolean hasAdmin() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return false;
        }
        SpringSecurityUser userDetails = (SpringSecurityUser) o;
        if (CollectionUtils.isEmpty(userDetails.getRoles())) {
            return false;
        }
        return userDetails.getRoles().stream().anyMatch(item -> SystemRole.ADMIN.name().equals(item.getName()));
    }

    public SpringSecurityUser loginUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return null;
        }
        return (SpringSecurityUser) authentication.getPrincipal();
    }

    public String loginUsername() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return "";
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return "";
        }
        SpringSecurityUser springSecurityUser = (SpringSecurityUser) authentication.getPrincipal();
        if (Objects.isNull(springSecurityUser)) {
            return "";
        }
        return springSecurityUser.getUsername();
    }

    public void handlerRole(String roleInfo) {
        authorizationConfigService.handlerRole(roleInfo);
        syncAuthorizationConfig();
    }

    public void syncAuthorizationConfig() {
        AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
        SyncAuthorizationDto syncAuthorizationDto = new SyncAuthorizationDto(authorizationConfigDto, SyncAuthorizationEnum.UPDATE);
        clusterSyncService.syncAuthorization(syncAuthorizationDto);
    }

    public ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize, Integer queueCapacity, Integer keepAliveSeconds, String threadNamePrefix, Integer awaitTerminationSeconds, RejectedExecutionHandler rejectedExecutionHandler) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        int availableCores = getAvailableCores();
        log.info("Current available cpu cores [{}]", availableCores);
        if (availableCores < 8) {
            availableCores = 8;
            log.info("Modify available cpu cores [{}]", availableCores);
        }
        if (corePoolSize > availableCores) {
            executor.setCorePoolSize(availableCores);
            executor.setMaxPoolSize(availableCores);
        } else {
            executor.setCorePoolSize(corePoolSize);
            executor.setMaxPoolSize(maxPoolSize);
        }
        Integer maxQueueCapacity = 100000000;
        if (queueCapacity > maxQueueCapacity) {
            queueCapacity = maxQueueCapacity;
        }
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(keepAliveSeconds);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(awaitTerminationSeconds);
        if (Objects.isNull(rejectedExecutionHandler)) {
            rejectedExecutionHandler = new ThreadPoolExecutor.CallerRunsPolicy();
        }
        executor.setRejectedExecutionHandler(rejectedExecutionHandler);
        executor.initialize();
        log.info("Thread pool name [{}] core size [{}] max size [{}] queue capacity [{}]", executor.getThreadNamePrefix(), executor.getCorePoolSize(), executor.getMaxPoolSize(), queueCapacity);
        return executor;
    }

    public int getAvailableCores() {
        return Runtime.getRuntime().availableProcessors();
    }

    public void putWsNode(String targetHostName) {
        String wsNodes = distributedCacheComponent.get(GlobalConstants.WS_NODE_KEY);
        List<String> wsNodeList = Lists.newArrayList();
        if (StringUtils.isNotBlank(wsNodes)) {
            wsNodeList = Lists.newArrayList(wsNodes.split(","));
        }
        String lockIp = folibLockProperties.getFolibLockIp();
        if (StringUtils.isNotBlank(lockIp)) {
            int port = System.getProperty("folib.port") != null ?
                    Integer.parseInt(System.getProperty("folib.port")) :
                    38080;
            String wsNode = String.format("%s_http://%s:%s", targetHostName, lockIp, port);
            if (!wsNodeList.contains(wsNode)) {
                wsNodeList.add(wsNode);
            }
            String value = String.join(",", wsNodeList);
            distributedCacheComponent.put(GlobalConstants.WS_NODE_KEY, value);
            log.info("Cache WS node [{}]", value);
        }
    }

    public void removeWsNode(String targetHostName) {
        String wsNodes = distributedCacheComponent.get(GlobalConstants.WS_NODE_KEY);
        if (StringUtils.isBlank(wsNodes)) {
            return;
        }
        List<String> wsNodeList = Lists.newArrayList(wsNodes.split(","));
        String lockIp = folibLockProperties.getFolibLockIp();
        if (StringUtils.isNotBlank(lockIp)) {
            int port = System.getProperty("folib.port") != null ?
                    Integer.parseInt(System.getProperty("folib.port")) :
                    38080;
            String wsNode = String.format("%s_http://%s:%s", targetHostName, lockIp, port);
            wsNodeList.remove(wsNode);
            String value = String.join(",", wsNodeList);
            distributedCacheComponent.put(GlobalConstants.WS_NODE_KEY, value);
            log.info("Cache WS node [{}]", value);
        }
    }
}
