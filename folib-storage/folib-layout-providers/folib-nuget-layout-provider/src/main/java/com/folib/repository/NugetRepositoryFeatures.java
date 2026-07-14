package com.folib.repository;

import com.folib.config.CustomAuthenticationFeature;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.repositories.ArtifactRepository;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.WebTarget;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;


@Component
public class NugetRepositoryFeatures
        implements RepositoryFeatures
{

    private static final int REMOTE_FEED_PAGE_SIZE = 1000;

    private static final Logger logger = LoggerFactory.getLogger(NugetRepositoryFeatures.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RedeploymentValidator redeploymentValidator;

    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;

    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;

    @Inject
    private ArtifactIdGroupService artifactIdGroupService;
    
    private Set<String> defaultMavenArtifactCoordinateValidators;

    @Inject
    private ArtifactRepository artifactRepository;

    @PostConstruct
    public void init()
    {
        defaultMavenArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                                                                                     genericReleaseVersionValidator.getAlias(),
                                                                                     genericSnapshotVersionValidator.getAlias()));
    }

    protected Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultMavenArtifactCoordinateValidators;
    }

    /**
     * Client WebTarget 构建认证信息
     *
     * @param webTarget webTarget
     * @param username  username
     * @param password  password
     */
    public void authentication(WebTarget webTarget, String username, String password) {
        final CustomAuthenticationFeature customAuthenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? CustomAuthenticationFeature.create(username, password) : null;
        if (customAuthenticationFeature != null) {
            webTarget.register(customAuthenticationFeature);
            webTarget.property(ApacheClientProperties.REQUEST_CONFIG,
                    RequestConfig.custom().setCircularRedirectsAllowed(true).build());
        }
    }

}
