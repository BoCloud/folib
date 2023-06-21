package com.veadan.folib.users.service.impl;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import javax.inject.Inject;
import javax.inject.Qualifier;
import javax.transaction.Transactional;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.veadan.folib.data.CacheName;
import com.veadan.folib.domain.PageResultResponse;
import com.veadan.folib.domain.SecurityRole;
import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.domain.UserData;
import com.veadan.folib.users.domain.Users;
import com.veadan.folib.users.security.JwtAuthenticationClaimsProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.users.userdetails.UserDetailsMapper;
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

    @Inject
    private UserDetailsMapper userDetailsMapper;

    @Inject
    @JwtAuthenticationClaimsProvider.JwtAuthentication
    private JwtClaimsProvider jwtClaimsProvider;

    @Override
    @CacheEvict(cacheNames = CacheName.User.AUTHENTICATIONS, key = "#p0")
    public void deleteByUsername(String username)
    {
        userRepository.deleteById(username);
    }

    @Override
    public List<User> findUserByRoles(List<String> rolesList) {
        return userRepository.findUsersWithRoles(rolesList);
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
        SpringSecurityUser springSecurityUser = userDetailsMapper.apply(user);
        Map<String, String> claimMap = jwtClaimsProvider.getClaims(springSecurityUser);
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

        if (StringUtils.isNotBlank(userToUpdate.getOriginalPassword()))
        {
            user.setOriginalPassword(userToUpdate.getOriginalPassword());
        }

        if (StringUtils.isNotBlank(userToUpdate.getSecurityTokenKey()))
        {
            user.setSecurityTokenKey(userToUpdate.getSecurityTokenKey());
        }

        if (!StringUtils.isBlank(userToUpdate.getAvatar()))
        {
            user.setAvatar(userToUpdate.getAvatar());
        }

        if (!StringUtils.isBlank(userToUpdate.getEmail()))
        {
            user.setEmail(userToUpdate.getEmail());
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
    public PageResultResponse<User> queryUser(User user, Integer page, Integer limit) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        int start = (page - 1) * limit;
        limit = page * limit;
        long count = userRepository.countUsers(user);
        if (count == 0L) {
            return null;
        }
        List<User> userList = userRepository.findUsersPage(user, start, limit);
        return new PageResultResponse<User>(count, userList);
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
        if (StringUtils.isNotBlank(user.getOriginalPassword()))
        {
            userEntity.setOriginalPassword(user.getOriginalPassword());
        }
        userEntity.setEnabled(user.isEnabled());
        List<String> showRoleNameList = Lists.newArrayList(SystemRole.ADMIN.name(), SystemRole.ARTIFACTS_MANAGER.name(), SystemRole.GENERAL.name(), SystemRole.OPEN_SOURCE_MANAGE.name());
        Set<SecurityRole> roles = Optional.ofNullable(userEntity.getRoles()).orElse(Sets.newLinkedHashSet()).stream().filter(item -> !showRoleNameList.contains(item.getRoleName())).collect(Collectors.toSet());
        roles.addAll(user.getRoles());
        userEntity.setRoles(roles);
        userEntity.setSecurityTokenKey(user.getSecurityTokenKey());
        userEntity.setEmail(user.getEmail());
        userEntity.setLastUpdated(now);
        userEntity.setUserType("general");
        userEntity.setAvatar(user.getAvatar());

//        if (StringUtils.isNotBlank(user.getSourceId()) || StringUtils.isNotBlank(userEntity.getSourceId()))
//        {
//            throw new IllegalStateException("Can't modify external users.");
//        }
        
        return userRepository.save(userEntity);
    }

    @Override
    public User saveOverrideRole(User user) {
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
