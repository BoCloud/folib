package com.veadan.folib.authentication;

import com.veadan.folib.data.CacheName;
import com.veadan.folib.domain.User;
import com.veadan.folib.users.domain.UserData;
import com.veadan.folib.users.service.UserAlreadyExistsException;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.userdetails.StrongboxExternalUsersCacheManager;
import com.veadan.folib.users.userdetails.StrongboxUserDetails;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.util.LocalDateTimeInstance;

import javax.transaction.Transactional;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.janusgraph.core.SchemaViolationException;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

/**
 * @author sbespalov
 */
@Component
@Transactional
public class DatabaseExternalUsersCacheManager extends DatabaseUserService implements StrongboxExternalUsersCacheManager
{


    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p1.username")
    public User cacheExternalUserDetails(String sourceId,
                                         UserDetails springUser)
    {
        User user = springUser instanceof StrongboxUserDetails ? ((StrongboxUserDetails) springUser).getUser()
                : new UserData(springUser);
        String username = user.getUsername();
        
        Optional<UserEntity> optionalUser = Optional.ofNullable(findByUsername(user.getUsername()));
        try
        {
            // If found user was from another source then remove before save
            if (optionalUser.map(User::getSourceId).map(sourceId::equals).filter(Boolean.FALSE::equals).isPresent())
            {
                deleteByUsername(optionalUser.map(u -> user.getUuid()).get());
                optionalUser = Optional.empty();
            }
            
            UserEntity userEntry = optionalUser.orElseGet(() -> new UserEntity(username));
            
            if (!StringUtils.isBlank(user.getPassword()))
            {
                userEntry.setPassword(user.getPassword());
            }
            userEntry.setEnabled(user.isEnabled());
            userEntry.setRoles(user.getRoles());
            userEntry.setSecurityTokenKey(user.getSecurityTokenKey());
            userEntry.setLastUpdated(LocalDateTimeInstance.now());
            userEntry.setSourceId(sourceId);

            return userRepository.save(userEntry);
        }
        catch (SchemaViolationException e)
        {
            
            throw new UserAlreadyExistsException(String.format("Failed to cache external user from [%s], duplicate [%s] already exists.", sourceId,
                                                               user.getUsername()),
                                                 e);
        }
    }

    
}
