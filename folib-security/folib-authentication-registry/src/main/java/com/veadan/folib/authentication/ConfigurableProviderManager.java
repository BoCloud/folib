package com.veadan.folib.authentication;

import com.veadan.folib.authentication.api.AuthenticationItem;
import com.veadan.folib.authentication.api.AuthenticationItemConfigurationManager;
import com.veadan.folib.authentication.api.AuthenticationItems;
import com.veadan.folib.authentication.api.CustomAuthenticationItemMapper;
import com.veadan.folib.authentication.registry.AuthenticationProvidersRegistry;
import com.veadan.folib.authentication.support.AuthenticationConfigurationContext;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.User;
import com.veadan.folib.entity.FolibUser;
import com.veadan.folib.enums.LoginTypeEnum;
import com.veadan.folib.mapper.FolibUserMapper;
import com.veadan.folib.users.service.UserAlreadyExistsException;
import com.veadan.folib.users.userdetails.FolibExternalUsersCacheManager;
import com.veadan.folib.users.userdetails.UserDetailsMapper;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Primary;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.transaction.Transactional;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author xuxinping
 */
@Primary
@Component
public class ConfigurableProviderManager extends ProviderManager implements UserDetailsService, AuthenticationItemConfigurationManager {

    private static final Logger logger = LoggerFactory.getLogger(ConfigurableProviderManager.class);

    @Inject
    private AuthenticationProvidersRegistry authenticationProvidersRegistry;

    @Inject
    private UserDetailsMapper userDetailsMapper;

    @Inject
    private FolibExternalUsersCacheManager folibExternalUsersCacheManager;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    private FolibUserMapper folibUserMapper;

    private final Map<String, AuthenticationProvider> authenticationProviderMap = new HashMap<>();

    private final Map<String, UserDetailsService> userProviderMap = new LinkedHashMap<>();

    public ConfigurableProviderManager() {
        super(new ArrayList<>(), new EmptyAuthenticationManager());
    }

    public void reload()
            throws IOException {
        logger.info("Reloading Authentication.");

        authenticationProvidersRegistry.reload();

        reloadAuthenticationItems();
    }

    private void reloadAuthenticationItems() {
        authenticationProviderMap.clear();
        authenticationProviderMap.putAll(authenticationProvidersRegistry.getAuthenticationProviderMap());

        userProviderMap.clear();
        userProviderMap.putAll(authenticationProvidersRegistry.getUserDetailsServiceMap());
    }

    @Override
    public List<AuthenticationProvider> getProviders() {
        return new ArrayList<>(authenticationProviderMap.values());
    }

    @Override
    @Transactional
    public UserDetails loadUserByUsername(String username)
            throws UsernameNotFoundException {
        return loadUserDetails(username).map(userDetailsMapper)
                .orElseThrow(() -> new UsernameNotFoundException(username));
    }

    private Optional<User> loadUserDetails(String username) {
        Optional<User> optionalUser = Optional.ofNullable(folibExternalUsersCacheManager.findByUsername(username)).filter(this::isInternalOrValidExternalUser);
        if (optionalUser.isPresent()) {
            return optionalUser;
        }

        return loadExternalUserDetails(username);
    }

    protected Optional<User> loadExternalUserDetails(String username) {
        FolibUser folibUser = folibUserMapper.selectByPrimaryKey(username);
        if (Objects.nonNull(folibUser) && StringUtils.isNotBlank(folibUser.getSourceId())) {
            String sourceId = folibUser.getSourceId();
            UserDetailsService userDetailsService = userProviderMap.get(sourceId);
            UserDetails externalUser = null;
            try {
                externalUser = userDetailsService.loadUserByUsername(username);
            } catch (UsernameNotFoundException e) {
                logger.info("User [{}] not found from [{}]", username, sourceId);
            }
            try {
                return Optional.of(folibExternalUsersCacheManager.cacheExternalUserDetails(sourceId, externalUser));
            } catch (UserAlreadyExistsException e) {
                logger.info(String.format("Retry to load user [%s] from [%s] by reason [%s]",
                        username, sourceId, e.getMessage()));
                return loadUserDetails(username);
            }
        } else {
            HttpServletRequest httpServletRequest = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getRequest();
            String loginType = httpServletRequest.getHeader("X-Folibrary-Login-Type");
            String ldapUserDetailsServiceSourceId = "ldapUserDetailsService";
            for (Map.Entry<String, UserDetailsService> userDetailsServiceEntry : userProviderMap.entrySet()) {
                String sourceId = userDetailsServiceEntry.getKey();
                if (ldapUserDetailsServiceSourceId.equals(sourceId) && !LoginTypeEnum.LDAP.getType().equalsIgnoreCase(loginType)) {
                    continue;
                }
                UserDetailsService userDetailsService = userDetailsServiceEntry.getValue();

                UserDetails externalUser;
                try {
                    externalUser = userDetailsService.loadUserByUsername(username);
                } catch (UsernameNotFoundException e) {
                    logger.info("User [{}] not found from [{}]", username, sourceId);
                    continue;
                }
                try {
                    return Optional.of(folibExternalUsersCacheManager.cacheExternalUserDetails(sourceId, externalUser));
                } catch (UserAlreadyExistsException e) {
                    logger.info(String.format("Retry to load user [%s] from [%s] by reason [%s]",
                            username, sourceId, e.getMessage()));

                    return loadUserDetails(username);
                }
            }
        }
        return Optional.empty();
    }

    private boolean isInternalOrValidExternalUser(User user) {
        Integer timeout = configurationManager.getSessionTimeoutSeconds();

        LocalDateTime userLastUpdate = Optional.ofNullable(user.getLastUpdated())
                .orElse(LocalDateTime.MIN);
        LocalDateTime userExpireDate = userLastUpdate.plusSeconds(timeout);
        LocalDateTime nowDate = LocalDateTimeInstance.now();

        return false;
    }

    @Override
    public Authentication authenticate(Authentication authentication)
            throws AuthenticationException {
        try {
            return super.authenticate(authentication);
        } catch (InternalAuthenticationServiceException e) {
            logger.error(String.format("Failed to authenticate user [%s]", authentication.getName()), e);
            throw e;
        }
    }

    public void reorder(String first,
                        String second)
            throws IOException {
        Map<String, Object> p1 = authenticationProvidersRegistry.getAuthenticationProperties(first);
        Map<String, Object> p2 = authenticationProvidersRegistry.getAuthenticationProperties(second);

        assertValidConfiguration(first, p1);
        assertValidConfiguration(second, p2);

        Integer orderFirst = (Integer) p1.get("order");
        Integer orderSecond = (Integer) p2.get("order");

        p1.put("order", orderSecond);
        p2.put("order", orderFirst);

        if (!authenticationProvidersRegistry.mergeProperties()
                .merge(first, p1)
                .merge(second, p2)
                .apply()) {
            return;
        }

        reloadAuthenticationItems();
    }

    @Override
    public void updateAuthenticationItems(AuthenticationItems items)
            throws IOException {

        AuthenticationProvidersRegistry.MergePropertiesContext mergePropertiesContext = authenticationProvidersRegistry.mergeProperties();
        for (AuthenticationItem item : items.getAuthenticationItemList()) {
            Map<String, Object> properties = authenticationProvidersRegistry.getAuthenticationProperties(item.getName());

            properties.put("order", item.getOrder());
            properties.put("enabled", Boolean.TRUE.equals(item.getEnabled()));

            mergePropertiesContext = mergePropertiesContext.merge(item.getName(), properties);

        }

        boolean result = mergePropertiesContext.apply();
        if (!result) {
            return;
        }

        reloadAuthenticationItems();
    }

    private void assertValidConfiguration(String authenticationItemId,
                                          Map<String, Object> p1) {
        if (p1 == null || p1.get("order") == null) {
            throw new IllegalArgumentException(
                    String.format("Invalid authentication configuration for [%s]", authenticationItemId));
        }
    }

    @Override
    public AuthenticationItems getAuthenticationItems() {
        Map<String, Object> authenticationProperties = authenticationProvidersRegistry.getAuthenticationProperties();

        AuthenticationItems result = new AuthenticationItems();
        result.getAuthenticationItemList()
                .addAll(Stream.concat(authenticationItemStream(AuthenticationProvider.class, authenticationProviderMap,
                        authenticationProperties),
                        authenticationItemStream(UserDetailsService.class, userProviderMap,
                                authenticationProperties))
                        .collect(Collectors.toList()));

        return result;
    }

    @Override
    public <T> T getCustomAuthenticationItem(CustomAuthenticationItemMapper<T> mapper) {
        Map<String, Object> map = authenticationProvidersRegistry.getAuthenticationProperties(mapper.getConfigurationItemId());

        return mapper.map(map);
    }

    @Override
    public <T> void putCustomAuthenticationItem(T customAuthenticationItem,
                                                CustomAuthenticationItemMapper<T> mapper)
            throws IOException {
        String itemId = mapper.getConfigurationItemId();

        authenticationProvidersRegistry.mergeProperties()
                .merge(itemId, mapper.map(customAuthenticationItem))
                .apply();
    }

    @Override
    public <T> void testCustomAuthenticationItem(T customAuthenticationItem,
                                                 CustomAuthenticationItemMapper<T> mapper,
                                                 Predicate<ApplicationContext> p)
            throws IOException {
        String itemId = mapper.getConfigurationItemId();

        authenticationProvidersRegistry.mergeProperties()
                .merge(itemId, mapper.map(customAuthenticationItem))
                .apply(p);
    }

    private <T> Stream<AuthenticationItem> authenticationItemStream(Class<T> itemClass,
                                                                    Map<String, T> sourceMap,
                                                                    Map<String, Object> authenticationProperties) {
        return sourceMap.entrySet()
                .stream()
                .map(e -> new AuthenticationItem(
                        e.getKey(),
                        itemClass.getSimpleName(),
                        (Map<String, Object>) authenticationProperties.get(e.getKey())))
                .sorted((i1,
                         i2) -> i1.getOrder()
                        .compareTo(i2.getOrder()));
    }

    @EventListener({ContextRefreshedEvent.class})
    public void contextRefreshedEvent(ContextRefreshedEvent e)
            throws IOException {
        ApplicationContext applicationContext = e.getApplicationContext();
        if (applicationContext == null || applicationContext instanceof AuthenticationConfigurationContext) {
            return;
        }

        reload();
    }

    public static final class EmptyAuthenticationManager implements AuthenticationManager {

        @Override
        public Authentication authenticate(Authentication authentication)
                throws AuthenticationException {
            return null;
        }

    }

    public static final class AuthenticationNotConfiguredException extends AuthenticationException {

        public AuthenticationNotConfiguredException() {
            super("Authentication should be configured with `folib-authentication-providers.xml`");
        }

    }

}
