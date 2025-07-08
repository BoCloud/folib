package com.folib.authentication;


import com.folib.data.CacheName;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.users.domain.UserData;
import com.folib.users.service.UserAlreadyExistsException;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.users.userdetails.FolibExternalUsersCacheManager;
import com.folib.users.userdetails.FolibUserDetails;
import com.folib.util.LocalDateTimeInstance;
import jakarta.transaction.Transactional;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.janusgraph.core.SchemaViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * @author veadan
 */
@Component
@Transactional
public class DatabaseExternalUsersCacheManager extends RelationalDatabaseUserService implements FolibExternalUsersCacheManager {

    private static final Logger logger = LoggerFactory.getLogger(DatabaseExternalUsersCacheManager.class);

    @Override
    public UserEntity findByUsername(String username) {
        UserEntity result = super.findByUsername(username);
        if (result != null) {
            logger.debug("User found in DB: username=[{}], sourceId=[{}], id=[{}], uuid=[{}]",
                    result.getUsername(),
                    result.getSourceId(),
                    result.getNativeId(),
                    result.getUuid());
        } else {
            logger.info("User not found in DB: username=[{}]", username);
        }

        return result;
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p1.username")
    public User cacheExternalUserDetails(String sourceId,
                                         UserDetails springUser) {
        User user;
        if (springUser instanceof FolibUserDetails) {
            user = ((FolibUserDetails) springUser).getUser();
        } else {
            user = new UserData(springUser);
        }
        String username = user.getUsername();
        Optional<UserEntity> oldUser = Optional.ofNullable(findByUsername(user.getUsername()));
        try {
            // If found user was from another source then remove before save
            Optional<String> oldSource = oldUser.map(User::getSourceId);
            if (oldSource.map(sourceId::equals).filter(Boolean.FALSE::equals).isPresent()) {
//                logger.info("Invalidate user from another source: username=[{}], oldSource=[{}], newSource=[{}]",
//                        username,
//                        oldSource.get(),
//                        sourceId);
//                deleteByUsername(oldUser.map(u -> user.getUuid()).get());
//                oldUser = Optional.empty();
                logger.info("user was from another source");
                throw new RuntimeException("invalid.credentials");
            }

            UserEntity userEntry = oldUser.orElseGet(() -> new UserEntity(username));
            String ldapUserDetailsServiceSourceId = "ldapUserDetailsService";
            if (!ldapUserDetailsServiceSourceId.equalsIgnoreCase(sourceId) && !StringUtils.isBlank(user.getPassword())) {
                userEntry.setPassword(user.getPassword());
            }
            userEntry.setUsername(username);
            userEntry.setEmail(user.getEmail());
            userEntry.setEnabled(user.isEnabled());
            if (CollectionUtils.isEmpty(userEntry.getRoles())) {
                userEntry.setRoles(user.getRoles());
            }
            userEntry.setSecurityTokenKey(user.getSecurityTokenKey());
            userEntry.setLastUpdated(LocalDateTimeInstance.now());
            userEntry.setSourceId(sourceId);

            save(userEntry);
            logger.debug("Cache external user: username=[{}], id=[{}], uuid=[{}], sourceId=[{}], lastUpdated=[{}], UserDetails=[{}]",
                    userEntry.getUsername(),
                    userEntry.getNativeId(),
                    userEntry.getUuid(),
                    userEntry.getSourceId(),
                    userEntry.getLastUpdated(),
                    springUser.getClass().getSimpleName());
            return userEntry;
        } catch (SchemaViolationException e) {
            throw new UserAlreadyExistsException(String.format("Failed to cache external user from [%s], duplicate [%s] already exists.", sourceId,
                    user.getUsername()),
                    e);
        }
    }


}
