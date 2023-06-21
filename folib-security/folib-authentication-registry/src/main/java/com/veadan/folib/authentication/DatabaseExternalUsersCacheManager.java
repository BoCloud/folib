package com.veadan.folib.authentication;


import com.veadan.folib.data.CacheName;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.users.domain.UserData;
import com.veadan.folib.users.service.UserAlreadyExistsException;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.userdetails.FolibExternalUsersCacheManager;
import com.veadan.folib.users.userdetails.FolibUserDetails;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.commons.lang3.StringUtils;
import org.janusgraph.core.SchemaViolationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import javax.transaction.Transactional;
import java.util.Optional;

/**
 * @author xuxinping
 */
@Component
@Transactional
public class DatabaseExternalUsersCacheManager extends DatabaseUserService implements FolibExternalUsersCacheManager {

    private static final Logger logger = LoggerFactory.getLogger(DatabaseExternalUsersCacheManager.class);

    @Override
    public UserEntity findByUsername(String username) {
        UserEntity result = super.findByUsername(username);
        if (result != null) {
            logger.info("User found in DB: username=[{}], sourceId=[{}], id=[{}], uuid=[{}]",
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
                logger.info("Invalidate user from another source: username=[{}], oldSource=[{}], newSource=[{}]",
                        username,
                        oldSource.get(),
                        sourceId);
                deleteByUsername(oldUser.map(u -> user.getUuid()).get());
                oldUser = Optional.empty();
            }

            UserEntity userEntry = oldUser.orElseGet(() -> new UserEntity(username));
            String ldapUserDetailsServiceSourceId = "ldapUserDetailsService";
            if(!ldapUserDetailsServiceSourceId.equalsIgnoreCase(sourceId) && !StringUtils.isBlank(user.getPassword())) {
                userEntry.setPassword(user.getPassword());
            }
            userEntry.setEmail(user.getEmail());
            userEntry.setEnabled(user.isEnabled());
            userEntry.setRoles(user.getRoles());
            userEntry.setSecurityTokenKey(user.getSecurityTokenKey());
            userEntry.setLastUpdated(LocalDateTimeInstance.now());
            userEntry.setSourceId(sourceId);

            UserEntity result = userRepository.save(userEntry);
            logger.info("Cache external user: username=[{}], id=[{}], uuid=[{}], sourceId=[{}], lastUpdated=[{}], UserDetails=[{}]",
                    result.getUsername(),
                    result.getNativeId(),
                    result.getUuid(),
                    result.getSourceId(),
                    result.getLastUpdated(),
                    springUser.getClass().getSimpleName());
            return result;
        } catch (SchemaViolationException e) {
            throw new UserAlreadyExistsException(String.format("Failed to cache external user from [%s], duplicate [%s] already exists.", sourceId,
                    user.getUsername()),
                    e);
        }
    }


}
