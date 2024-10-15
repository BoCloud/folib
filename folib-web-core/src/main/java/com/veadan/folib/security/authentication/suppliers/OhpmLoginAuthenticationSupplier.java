package com.veadan.folib.security.authentication.suppliers;

import com.veadan.folib.authentication.api.jwt.JwtAuthentication;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.enums.NpmSubLayout;
import com.veadan.folib.providers.layout.NpmLayoutProvider;
import com.veadan.folib.security.exceptions.InvalidTokenException;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.util.CacheUtil;
import com.veadan.folib.utils.UrlUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.util.Objects;

@Component
@Slf4j
public class OhpmLoginAuthenticationSupplier
        extends LayoutAuthenticationSupplier {

    public OhpmLoginAuthenticationSupplier() {
        super(NpmLayoutProvider.ALIAS);
    }

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    String AUTHORIZATION_HEADER = "Authorization";

    String USER_AGENT = "user-agent";

    String OHPM_USER_AGENT = "ohpm";

    String BEARER_AUTHORIZATION_PREFIX = "Bearer";

    String BASIC_AUTHORIZATION_PREFIX = "Basic";

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request) {
        String token = request.getHeader(AUTHORIZATION_HEADER);
        String storageId = UrlUtils.getCurrentStorageId();
        String repositoryId = UrlUtils.getCurrentRepositoryId();
        boolean hasHeader = StringUtils.isNotBlank(token) && !token.startsWith(BEARER_AUTHORIZATION_PREFIX) && !token.startsWith(BASIC_AUTHORIZATION_PREFIX) && isLayoutRepository(storageId, repositoryId);
        if (!hasHeader) {
            throw new BadCredentialsException("invalid.credentials");
        }
        String username;
        try {
            username = securityTokenProvider.getSubject(token);
        } catch (InvalidTokenException e) {
            log.error("OHPM token失效", e);
            throw new BadCredentialsException("invalid.token");
        }
        if (GlobalConstants.ANONYMOUS_TOKEN_KEY.equals(username)) {
            SecurityContext securityContext = SecurityContextHolder.getContext();
            Authentication authentication = securityContext.getAuthentication();
            if (authentication instanceof AnonymousAuthenticationToken) {
                return authentication;
            }
        }
        return new JwtAuthentication(username, token);
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request) {
        if (!super.supports(request)) {
            return false;
        }
        boolean hasHeader = false;
        if (StringUtils.isNotBlank(request.getHeader(AUTHORIZATION_HEADER)) && StringUtils.isNotBlank(request.getHeader(USER_AGENT))) {
            String authHeader = request.getHeader(AUTHORIZATION_HEADER);
            String storageId = UrlUtils.getCurrentStorageId();
            String repositoryId = UrlUtils.getCurrentRepositoryId();
            hasHeader = StringUtils.isNotBlank(authHeader) && !authHeader.startsWith(BEARER_AUTHORIZATION_PREFIX) && !authHeader.startsWith(BASIC_AUTHORIZATION_PREFIX) && isLayoutRepository(storageId, repositoryId);
        }
        if (hasHeader) {
            return true;
        }
        return false;
    }

    private boolean isLayoutRepository(String storageId, String repositoryId) {
        if (StringUtils.isBlank(storageId) || StringUtils.isBlank(repositoryId)) {
            return false;
        }
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
            if (Objects.isNull(storage)) {
                return false;
            }
            repository = storage.getRepository(repositoryId);
            if (Objects.isNull(repository)) {
                return false;
            }
            cacheUtil.put(key, repository);
        }
        return NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout());
    }
}
