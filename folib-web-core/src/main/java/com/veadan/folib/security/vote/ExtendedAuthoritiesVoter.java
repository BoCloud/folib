package com.veadan.folib.security.vote;

import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.controllers.BrowseController;
import com.veadan.folib.controllers.unicom.UnicomAdapter;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.CacheUtil;
import com.veadan.folib.utils.UrlUtils;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.access.ConfigAttribute;
import org.springframework.security.access.expression.method.ExpressionBasedPreInvocationAdvice;
import org.springframework.security.access.prepost.PreInvocationAuthorizationAdviceVoter;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import static com.veadan.folib.web.Constants.ARTIFACT_ROOT_PATH;
import static com.veadan.folib.web.Constants.DOCKER_ROOT_PATH;
import static com.veadan.folib.web.Constants.STORAGE_ROOT_PATH;

/**
 * @author xuxinping
 */
@Component
public class ExtendedAuthoritiesVoter extends PreInvocationAuthorizationAdviceVoter {

    private final Logger logger = LoggerFactory.getLogger(ExtendedAuthoritiesVoter.class);

    @Autowired
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    @Lazy
    private AuthoritiesProvider authoritiesProvider;

    @Autowired
    @Lazy
    private ConfigurationManager configurationManager;

    @Autowired
    @Lazy
    private ArtifactRepository artifactRepository;

    @Autowired
    @Lazy
    private UnicomAdapter unicomAdapter;


    public ExtendedAuthoritiesVoter() {
        super(new ExpressionBasedPreInvocationAdvice());
    }

    @Override
    public int vote(Authentication authentication,
                    MethodInvocation method,
                    Collection<ConfigAttribute> attributes) {
        return super.vote(new ExtendedAuthorityAuthentication(authentication), method, attributes);
    }

    @SuppressWarnings("serial")
    private class ExtendedAuthorityAuthentication implements Authentication {

        private Authentication source;

        public ExtendedAuthorityAuthentication(Authentication target) {
            super();
            this.source = target;
        }

        private Authentication getSourceAuthentication() {
            return source;
        }

        private Boolean getRepositoryAllowAnonymousFromCacheOrLoad(String storageId, String repositoryId) {
            CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
            String key = String.format("%s:%s", storageId, repositoryId);
            Repository repository = cacheUtil.get(key);
            if (Objects.isNull(repository)) {
                Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
                if (Objects.isNull(storage)) {
                    return true;
                }
                repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    return true;
                }
                cacheUtil.put(key, repository);
            }
            return repository.isAllowAnonymous();
        }

        private Collection<? extends GrantedAuthority> calculateExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path) {
            storageId = storageId == null ? UrlUtils.getCurrentStorageId() : storageId;
            repositoryId = repositoryId == null ? UrlUtils.getCurrentRepositoryId() : repositoryId;
            Object principal = authentication.getPrincipal();
            Collection<? extends GrantedAuthority> apiAuthorities = authentication.getAuthorities();
            logger.debug("Privileges for [{}] are [{}]", principal, apiAuthorities);
            String requestUri = path == null ? parseRequestUri(UrlUtils.getRequestUri()) : path;
            if (!authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
                if (!configurationManagementService.getConfiguration().getAdvancedConfiguration().isAllowAnonymous()) {
                    return Collections.emptySet();
                }
                Role anonymousRole = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
                Set<Privileges> anonymousApiAuthorities = anonymousRole.getAccessModel().getApiAuthorities();
                List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
                if (paths.stream().noneMatch(requestUri::startsWith)) {
                    return anonymousApiAuthorities;
                }
                if (storageId == null || repositoryId == null) {
                    return anonymousApiAuthorities;
                }
                Set<Privileges> storageAuthorities = anonymousRole.getAccessModel().getPathAuthorities(requestUri);
                List<GrantedAuthority> authorities = new ArrayList<>(anonymousApiAuthorities);
                if (storageAuthorities.isEmpty()) {
                    return anonymousApiAuthorities;
                } else {
                    authorities.remove(Privileges.ARTIFACTS_RESOLVE);
                }
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                    if (Boolean.FALSE.equals(getRepositoryAllowAnonymousFromCacheOrLoad(storageId, repositoryId))) {
                        return Collections.emptySet();
                    }
                }
                authorities.addAll(storageAuthorities);
                return authorities;
            } else if (!(principal instanceof SpringSecurityUser)) {
                logger.warn("Unknown authentication principal type [{}]", principal.getClass());
                return authentication.getAuthorities();
            }
            List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
            if (paths.stream().noneMatch(requestUri::startsWith)) {
                return apiAuthorities;
            }
            if (storageId == null || repositoryId == null) {
                return apiAuthorities;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            // 资源权限
            if (configurationManager.getStorage(storageId) == null || configurationManager.getRepository(storageId, repositoryId) == null) {
                if (UnicomAdapter.UNICOM_SOURCE_ID.equals(userDetails.getSourceId())) {
                    Set<GrantedAuthority> unicomAuthorities = new HashSet<>();
                    unicomAuthorities.add(Privileges.ARTIFACTS_DEPLOY);
                    unicomAuthorities.addAll(apiAuthorities);
                    return unicomAuthorities;
                } else {
                    return apiAuthorities;
                }

            }

            Repository repository = configurationManager.getRepository(storageId, repositoryId);
            // 判断是否为组合库
            if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                String storeAndRepo = storageId + "/" + repositoryId + "/";
                int index = requestUri.indexOf(storeAndRepo);
                String relativePath = "";
                if (index != -1) {
                    relativePath = requestUri.substring(index + storeAndRepo.length());
                }
                // 获取所有子仓库
                List<String> storageAndRepositoryIds = new LinkedList<>();
                configurationManager.resolveGroupRepository(repository, storageAndRepositoryIds);
                Set<GrantedAuthority> extendedAuthorities = new HashSet<>();
                for (String storageAndRepositoryId : storageAndRepositoryIds) {
                    String subStorageId = ConfigurationUtils.getStorageId(storageId, storageAndRepositoryId);
                    String subRepositoryId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                    String newPath = rewriteByStoreAndRepo(requestUri, subStorageId, subRepositoryId);
                    Repository subrepository = configurationManager.getRepository(subStorageId, subRepositoryId);
                    // 如果是本地库判断该仓库是否有该制品
                    if (RepositoryTypeEnum.HOSTED.getType().equals(subrepository.getType())) {
                        if (artifactRepository.artifactExists(subStorageId, subRepositoryId, relativePath)) {
                            Collection<? extends GrantedAuthority> grantedAuthorities = calculateExtendedAuthorities(authentication, subStorageId, subRepositoryId, newPath);
                            extendedAuthorities.addAll(grantedAuthorities);
                        }
                    } else {
                        Collection<? extends GrantedAuthority> grantedAuthorities = calculateExtendedAuthorities(authentication, subStorageId, subRepositoryId, newPath);
                        extendedAuthorities.addAll(grantedAuthorities);
                    }
                }
                return extendedAuthorities;
            } else {

                if (UnicomAdapter.UNICOM_SOURCE_ID.equals(userDetails.getSourceId())) {
                    String email = userDetails.getEmail();
                    Set<String> projects = unicomAdapter.getUserDetail(email).ownProject();
                    Repository repo = configurationManager.getRepository(storageId, repositoryId);
                    if (projects.contains(repo.getProjectId())) {
                        List<GrantedAuthority> authorities = new ArrayList<>(apiAuthorities);
                        authorities.addAll(Privileges.artifactsAll());
                        return authorities;
                    } else {
                        return apiAuthorities;
                    }
                }
                Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(requestUri);
                if (storageAuthorities.isEmpty()) {
                    return apiAuthorities;
                }
                List<GrantedAuthority> extendedAuthorities = new ArrayList<>(apiAuthorities);
                extendedAuthorities.addAll(storageAuthorities);
                logger.debug("Privileges for [{}] was extended to [{}]", userDetails.getUsername(), extendedAuthorities);
                return extendedAuthorities;
            }
        }

        @Override
        public String getName() {
            return getSourceAuthentication().getName();
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return calculateExtendedAuthorities(getSourceAuthentication(), null, null, null);
        }

        @Override
        public Object getCredentials() {
            return getSourceAuthentication().getCredentials();
        }

        @Override
        public Object getDetails() {
            return getSourceAuthentication().getDetails();
        }

        @Override
        public Object getPrincipal() {
            return getSourceAuthentication().getPrincipal();
        }

        @Override
        public boolean isAuthenticated() {
            return getSourceAuthentication().isAuthenticated();
        }

        @Override
        public void setAuthenticated(boolean isAuthenticated)
                throws IllegalArgumentException {
            getSourceAuthentication().setAuthenticated(isAuthenticated);
        }
    }

    String rewriteByStoreAndRepo(String path, String storageId, String repositoryId) {
        String[] split = path.split("/");
        if (split.length <= 4) {
            return path;
        } else {
            split[2] = storageId;
            split[3] = repositoryId;
            return String.join("/", split);
        }
    }

    private String parseRequestUri(String requestUri) {
        try {
            requestUri = UriUtils.decode(requestUri);
        } catch (Exception ex) {
            logger.error("Get requestUri error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return requestUri;
    }

}
