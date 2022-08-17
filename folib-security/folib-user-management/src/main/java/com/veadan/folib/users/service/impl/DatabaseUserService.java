package com.veadan.folib.users.service.impl;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import javax.inject.Inject;
import javax.inject.Qualifier;
import javax.transaction.Transactional;

import com.veadan.folib.data.CacheName;
import com.veadan.folib.users.domain.UserData;
import com.veadan.folib.users.domain.Users;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.UserService;
import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.domain.User;
import com.veadan.folib.domain.UserEntity;
import com.veadan.folib.repositories.UserRepository;
import com.veadan.folib.users.service.impl.DatabaseUserService.Database;
import com.veadan.folib.util.LocalDateTimeInstance;

import org.jose4j.lang.JoseException;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Component;

/**
 * @author xuxinping
 */
@Component
@Database
@Transactional
public class DatabaseUserService implements UserService
{

    @Inject
    private SecurityTokenProvider tokenProvider;
    
    @Inject
    protected UserRepository userRepository;


    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0")
    public void deleteByUsername(String username)
    {
        userRepository.deleteById(username);
    }

    @Override
    public UserEntity findByUsername(String username)
    {
        return userRepository.findById(username).map(UserEntity.class::cast).orElse(null);
    }

    @Override
    public String generateSecurityToken(String username)
        throws JoseException
    {
        final User user = findByUsername(username);

        if (StringUtils.isEmpty(user.getSecurityTokenKey()))
        {
            return null;
        }

        final Map<String, String> claimMap = new HashMap<>();
        claimMap.put(UserData.SECURITY_TOKEN_KEY, user.getSecurityTokenKey());

        return tokenProvider.getToken(username, claimMap, null, null);
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    public void updateAccountDetailsByUsername(User userToUpdate)
    {
        UserEntity user = findByUsername(userToUpdate.getUsername());
        if (user == null)
        {
            throw new UsernameNotFoundException(userToUpdate.getUsername());
        }

        if (!StringUtils.isBlank(userToUpdate.getPassword()))
        {
            user.setPassword(userToUpdate.getPassword());
        }

        if (StringUtils.isNotBlank(userToUpdate.getSecurityTokenKey()))
        {
            user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());
        }

        save(user);
    }

    @Override
    public Users getUsers()
    {
        Iterable<User> users = userRepository.findAll();
        return new Users(StreamSupport.stream(users.spliterator(), false).collect(Collectors.toSet()));
    }

    @Override
    public void revokeEveryone(String roleToRevoke)
    {
        List<User> resultList = userRepository.findUsersWithRole(roleToRevoke);

        resultList.forEach(user -> {
            user.getRoles().remove(roleToRevoke);
            save(user);
        });
    }

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0.username")
    public User save(User user)
    {
        LocalDateTime now = LocalDateTimeInstance.now();

        UserEntity userEntity = Optional.ofNullable(findByUsername(user.getUsername())).orElseGet(() -> new UserEntity(user.getUsername()));

        if (!StringUtils.isBlank(user.getPassword()))
        {
            userEntity.setPassword(user.getPassword());
        }
        userEntity.setEnabled(user.isEnabled());
        userEntity.setRoles(user.getRoles());
        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");

//        if (StringUtils.isNotBlank(user.getSourceId()) || StringUtils.isNotBlank(userEntity.getSourceId()))
//        {
//            throw new IllegalStateException("Can't modify external users.");
//        }
        
        return userRepository.save(userEntity);
    }

    public void expireUser(String username, boolean clearSourceId)
    {
        UserEntity externalUserEntry = findByUsername(username);
        externalUserEntry.setLastUpdated(LocalDateTime.ofInstant(Instant.ofEpochMilli(0), ZoneId.systemDefault()));

        if (clearSourceId)
        {
            externalUserEntry.setSourceId("empty");
        }

        userRepository.save(externalUserEntry);
    }

    @Documented
    @Retention(RUNTIME)
    @Qualifier
    public @interface Database
    {
    }

}
