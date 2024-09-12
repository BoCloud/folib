package com.veadan.folib.users.security;

import cn.hutool.core.util.StrUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.domain.RoleData;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.AccessModelDto;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Component
@DependsOn("liquibase")
public class AuthoritiesProvider
{

    private static final Logger logger = LoggerFactory.getLogger(AuthoritiesProvider.class);

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;
    @Inject
    private DistributedCacheComponent distributedCacheComponent;
    @PostConstruct
    void init() throws IOException
    {
        final AuthorizationConfigDto config = authorizationConfigFileManager.read();
        authorizationConfigService.setAuthorizationConfig(config);
    }

    public Set<RoleData> getAssignableRoles()
    {
        return authorizationConfigService.get().getRoles();
    }
    
    public Role getRuntimeRole(String name)
    {
        RoleData role = new RoleData(new RoleDto(name, "", new AccessModelDto()));
        Set<RoleData> roles = authorizationConfigService.getRole(name).getRoles();
        if (CollectionUtils.isNotEmpty(roles)) {
            role = roles.stream().findFirst().orElseThrow(() -> new IllegalArgumentException(name));
        }

        if (SystemRole.ADMIN.name().equals(name))
        {
            RuntimeRole adminRole = new RuntimeRole(role, (a) -> new AdminAccessModel());
            return new RuntimeRole(adminRole, AuthenticatedAccessModel::new);
        }
        else if (SystemRole.ANONYMOUS.name().equals(name))
        {
            return new RuntimeRole(role, AnonymousAccessModel::new);
        }

        return new RuntimeRole(role, AuthenticatedAccessModel::new);
    }

    public Set<Role> getRuntimeRole(String roleId, String username)
    {
        String roleKey = String.format("user_role_%s", username);
        String roleStr = distributedCacheComponent.get(roleKey);
        Set<RoleData> roles;
        ObjectMapper objectMapper = new ObjectMapper();
        try {
            if (StrUtil.isEmpty(roleStr)) {
                roles = authorizationConfigService.get(username)
                        .getRoles();

                distributedCacheComponent.put(roleKey, objectMapper.writeValueAsString(roles));
            } else {
                List<RoleDto> roleDtos = objectMapper.readValue(roleStr, objectMapper.getTypeFactory().constructCollectionType(List.class, RoleDto.class));
                roles = roleDtos.stream().map(RoleData::new).collect(Collectors.toSet());
            }
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }

        Set<Role> roleSet = new HashSet<>();
        return roles.stream().map(r -> {
            if (SystemRole.ADMIN.name().equals(r.getName())) {
                RuntimeRole adminRole = new RuntimeRole(r, (a) -> new AdminAccessModel());
                roleSet.add(new RuntimeRole(adminRole, AuthenticatedAccessModel::new));
            }else if (SystemRole.ANONYMOUS.name().equals(r.getName())) {
                roleSet.add(new RuntimeRole(r, AnonymousAccessModel::new));
            }else {
                roleSet.add(new RuntimeRole(r, AuthenticatedAccessModel::new));
            }
            return roleSet;
        }).flatMap(Collection::stream).collect(Collectors.toSet());

    }

}
