package com.veadan.folib.security.vote;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.cloud.storage.s3fs.util.UriUtils;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.BrowseController;
import com.veadan.folib.enums.RepositoryScopeEnum;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.security.enums.ResolvePathTypeEnum;
import com.veadan.folib.security.resolvepath.ResolvePathProvider;
import com.veadan.folib.security.resolvepath.ResolvePathProviderRegistry;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.users.domain.AccessModelData;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.security.AnonymousAccessModel;
import com.veadan.folib.users.security.AuthoritiesProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.util.CacheUtil;
import com.veadan.folib.utils.UrlUtils;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.collections4.CollectionUtils;
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

import java.util.*;
import java.util.stream.Collectors;

import static com.veadan.folib.web.Constants.*;

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
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @Autowired
    @Lazy
    private RepositoryPathResolver repositoryPathResolver;


    public ExtendedAuthoritiesVoter() {
        super(new ExpressionBasedPreInvocationAdvice());
    }

    @Override
    public int vote(Authentication authentication,
                    MethodInvocation method,
                    Collection<ConfigAttribute> attributes) {
        return super.vote(new ExtendedAuthorityAuthentication(authentication), method, attributes);
    }

    public Collection<String> getExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path, Boolean enableSplitPath) {
        ExtendedAuthorityAuthentication extendedAuth = new ExtendedAuthorityAuthentication(authentication);
        return extendedAuth.calculateExtendedAuthorities(authentication, storageId, repositoryId, path, enableSplitPath).stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
    }

    public Collection<String> getExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path) {
        ExtendedAuthorityAuthentication extendedAuth = new ExtendedAuthorityAuthentication(authentication);
        return extendedAuth.calculateExtendedAuthorities(authentication, storageId, repositoryId, path, false).stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
    }

    public Repository getRepositoryFromCacheOrLoad(String storageId, String repositoryId) {
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (Objects.isNull(storage)) {
                return null;
            }
            repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                return null;
            }
            cacheUtil.put(key, repository);
        }
        return repository;
    }

    @SuppressWarnings("serial")
    public class ExtendedAuthorityAuthentication implements Authentication {

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

        public Collection<? extends GrantedAuthority> calculateExtendedAuthorities(Authentication authentication, String storageId, String repositoryId, String path, Boolean enableSplitPath) {
            String storageIdAndRepositoryId = "";
            if (StringUtils.isBlank(storageId)) {
                storageIdAndRepositoryId = UrlUtils.getCurrentStorageIdAndRepositoryId();
                if (StringUtils.isBlank(storageIdAndRepositoryId)) {
                    storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(UrlUtils.getCurrentStorageId(), UrlUtils.getCurrentRepositoryId());
                }
            }
            storageId = storageId == null ? ConfigurationUtils.getStorageId(storageIdAndRepositoryId, storageIdAndRepositoryId) : storageId;
            repositoryId = repositoryId == null ? ConfigurationUtils.getRepositoryId(storageIdAndRepositoryId) : repositoryId;
            Object principal = authentication.getPrincipal();
            Collection<? extends GrantedAuthority> apiAuthorities = authentication.getAuthorities();
            logger.debug("Privileges for [{}] are [{}]", principal, apiAuthorities);
            String requestUri = path == null ? parseRequestUri(UrlUtils.getRequestUri()) : path;
            Repository repository = getRepositoryFromCacheOrLoad(storageId, repositoryId);
            if (Objects.nonNull(repository)) {
                // 判断是否为组合库
                if (RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    // 获取所有子仓库
                    List<String> storageAndRepositoryIds = new LinkedList<>();
                    configurationManager.resolveGroupRepository(repository, storageAndRepositoryIds);
                    Set<GrantedAuthority> extendedAuthorities = new HashSet<>();
                    String storeAndRepo = storageId + "/" + repositoryId + "/";
                    int index = requestUri.indexOf(storeAndRepo);
                    String relativePath = "", sourceRelativePath = "";
                    if (index != -1) {
                        relativePath = requestUri.substring(index + storeAndRepo.length());
                        sourceRelativePath = relativePath;
                    }
                    String resolvePathType = ResolvePathTypeEnum.getResolvePathType(repository.getLayout());
                    if (StringUtils.isNotBlank(resolvePathType)) {
                        ResolvePathProvider resolvePathProvider = resolvePathProviderRegistry.getProvider(resolvePathType);
                        if (Objects.nonNull(resolvePathProvider)) {
                            relativePath = resolvePathProvider.resolvePath(repository, relativePath);
                        }
                    }
                    for (String storageAndRepositoryId : storageAndRepositoryIds) {
                        String subStorageId = ConfigurationUtils.getStorageId(storageId, storageAndRepositoryId);
                        String subRepositoryId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                        String newPath = rewriteByStoreAndRepo(requestUri, subStorageId, subRepositoryId);
                        if (StringUtils.isNotBlank(relativePath)) {
                            newPath = String.format("/storages/%s/%s/%s", subStorageId, subRepositoryId, sourceRelativePath);
                        }
                        Collection<? extends GrantedAuthority> grantedAuthorities = calculateExtendedAuthorities(authentication, subStorageId, subRepositoryId, newPath, enableSplitPath);
                        extendedAuthorities.addAll(grantedAuthorities);
                    }
                    return extendedAuthorities;
                }
                String resolvePathType = ResolvePathTypeEnum.getResolvePathType(repository.getLayout());
                if (StringUtils.isNotBlank(resolvePathType)) {
                    ResolvePathProvider resolvePathProvider = resolvePathProviderRegistry.getProvider(resolvePathType);
                    if (Objects.nonNull(resolvePathProvider)) {
                        requestUri = resolvePathProvider.resolvePath(repository, requestUri);
                    }
                }
            }
            if (!authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
                if (!configurationManagementService.getConfiguration().getAdvancedConfiguration().isAllowAnonymous()) {
                    return Collections.emptySet();
                }
                if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                    if (Boolean.FALSE.equals(getRepositoryAllowAnonymousFromCacheOrLoad(storageId, repositoryId))) {
                        return Collections.emptySet();
                    }
                }
                Role anonymousRole = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
                Set<Privileges> anonymousApiAuthorities = anonymousRole.getAccessModel().getApiAuthorities();
                List<GrantedAuthority> authorities = new ArrayList<>(anonymousApiAuthorities);
                AnonymousAccessModel anonymousAccessModel = (AnonymousAccessModel) anonymousRole.getAccessModel();
                AccessModelData accessModelData = (AccessModelData) anonymousAccessModel.getAccessModelTarget();
                if (CollectionUtils.isNotEmpty(accessModelData.getStorageAuthorities())) {
                    authorities.remove(Privileges.ARTIFACTS_RESOLVE);
                }
                if (Objects.nonNull(repository) && RepositoryScopeEnum.OPEN.getType().equals(repository.getScope())) {
                    authorities.add(Privileges.ARTIFACTS_RESOLVE);
                }
                List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, ARTIFACTORY_ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
                if (StringUtils.isNotBlank(requestUri) && paths.stream().noneMatch(requestUri::startsWith)) {
                    return authorities;
                }
                Set<Privileges> storageAuthorities = anonymousRole.getAccessModel().getPathAuthorities(requestUri, enableSplitPath);
                if (storageAuthorities.isEmpty()) {
                    return authorities;
                }
                authorities.addAll(storageAuthorities);
                return authorities;
            } else if (!(principal instanceof SpringSecurityUser)) {
                logger.warn("Unknown authentication principal type [{}]", principal.getClass());
                return authentication.getAuthorities();
            }
            List<GrantedAuthority> extendedAuthorities = new ArrayList<>(apiAuthorities);
            if (Objects.nonNull(repository) && RepositoryScopeEnum.OPEN.getType().equals(repository.getScope()) && !extendedAuthorities.contains(Privileges.ARTIFACTS_RESOLVE)) {
                extendedAuthorities.add(Privileges.ARTIFACTS_RESOLVE);
            }
            List<String> paths = Arrays.asList(ARTIFACT_ROOT_PATH, ARTIFACTORY_ARTIFACT_ROOT_PATH, DOCKER_ROOT_PATH, BrowseController.ROOT_CONTEXT, STORAGE_ROOT_PATH);
            if (StringUtils.isNotBlank(requestUri) && paths.stream().noneMatch(requestUri::startsWith)) {
                return extendedAuthorities;
            }
            SpringSecurityUser userDetails = (SpringSecurityUser) authentication.getPrincipal();
            Collection<Privileges> storageAuthorities = userDetails.getStorageAuthorities(requestUri, enableSplitPath);
            if (storageAuthorities.isEmpty()) {
                return extendedAuthorities;
            }
            extendedAuthorities.addAll(storageAuthorities);
            logger.debug("Privileges for [{}] was extended to [{}]", userDetails.getUsername(), extendedAuthorities);
            return extendedAuthorities;
        }

        @Override
        public String getName() {
            return getSourceAuthentication().getName();
        }

        @Override
        public Collection<? extends GrantedAuthority> getAuthorities() {
            return calculateExtendedAuthorities(getSourceAuthentication(), null, null, null, false);
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

    private String rewriteByStoreAndRepo(String path, String storageId, String repositoryId) {
        String[] split = path.split("/");
        if (split.length <= 4) {
            return path;
        } else {
            split[2] = storageId;
            split[3] = repositoryId;
            return String.join("/", split);
        }
    }

    private String parseArtifactoryRequestUri(String requestUri) {
        if (requestUri.startsWith(ARTIFACTORY_ARTIFACT_ROOT_PATH)) {
            for (String apiPrefix : ARTIFACT_API_ROOT_PATHS) {
                //使用api级别
                if (requestUri.startsWith(apiPrefix)) {
                    apiPrefix = requestUri.replace(apiPrefix, "");
                    String apiSuffix = StringUtils.removeStart(apiPrefix, GlobalConstants.SEPARATOR);
                    String storageId = parseStorageId(apiSuffix);
                    if (StringUtils.isBlank(storageId)) {
                        return requestUri;
                    }
                    requestUri = ARTIFACTORY_ARTIFACT_ROOT_PATH + GlobalConstants.SEPARATOR + storageId + GlobalConstants.SEPARATOR + apiSuffix;
                    return requestUri;
                }
            }
            requestUri = StringUtils.removeStart(requestUri.replace(ARTIFACTORY_ARTIFACT_ROOT_PATH, ""), GlobalConstants.SEPARATOR);
            String storageId = parseStorageId(requestUri);
            if (StringUtils.isBlank(storageId)) {
                return requestUri;
            }
            requestUri = ARTIFACTORY_ARTIFACT_ROOT_PATH + GlobalConstants.SEPARATOR + storageId + GlobalConstants.SEPARATOR + requestUri;
            return requestUri;
        }
        return requestUri;
    }

    private String parseStorageId(String path) {
        String repositoryId = path.split(GlobalConstants.SEPARATOR)[0];
        String storageId = getDefaultStorageId(repositoryId);
        if (Objects.isNull(getRepositoryFromCacheOrLoad(storageId, repositoryId))) {
            return "";
        }
        return storageId;
    }


    private String parseRequestUri(String requestUri) {
        try {
            requestUri = parseArtifactoryRequestUri(UriUtils.decode(requestUri));
        } catch (Exception ex) {
            logger.error("Get requestUri error [{}]", ExceptionUtils.getStackTrace(ex));
        }
        return requestUri;
    }

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
    }


}
