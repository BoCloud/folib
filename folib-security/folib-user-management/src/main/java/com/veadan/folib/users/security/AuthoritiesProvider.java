package com.veadan.folib.users.security;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.domain.RoleData;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.authorization.dto.RoleDto;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.entity.FolibRole;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.service.FolibRoleService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
public class AuthoritiesProvider
{

    private static final Logger logger = LoggerFactory.getLogger(AuthoritiesProvider.class);

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private DistributedCacheComponent distributedCacheComponent;
    @PostConstruct
    void init() throws IOException
    {
        final AuthorizationConfigDto config = authorizationConfigFileManager.read();
        authorizationConfigService.setAuthorizationConfig(config);
        FolibRole anonymous = folibRoleService.queryById("ANONYMOUS");
        if (anonymous == null){
            //同步角色
            folibRoleService.syncYamlAuthorizationConfig();
        }
    }

    public Set<RoleData> getAssignableRoles()
    {
        return authorizationConfigService.get().getRoles();
    }
    
    public Role getRuntimeRole(String name)
    {
        RoleData role = authorizationConfigService.get()
                                                  .getRoles()
                                                  .stream()
                                                  .filter(r -> r.getName().equalsIgnoreCase(name))
                                                  .findFirst()
                                                  .orElseThrow(() -> new IllegalArgumentException(name));

        if (SystemRole.ADMIN.name().equals(name))
        {
            RuntimeRole adminRole = new RuntimeRole(role, (a) -> new AdminAccessModel());
            return new RuntimeRole(adminRole, (a) -> new AuthenticatedAccessModel(a));
        }
        else if (SystemRole.ANONYMOUS.name().equals(name))
        {
            return new RuntimeRole(role, (a) -> new AnonymousAccessModel(a));
        }

        return new RuntimeRole(role, (a) -> new AuthenticatedAccessModel(a));
    }

    public Set<Role> getRuntimeRole(String roleId, String username)
    {
        String roleKey = String.format("user_role_%s", username);
        String roleStr = distributedCacheComponent.get(roleKey);
        Set<RoleData> roles;
        if (StrUtil.isEmpty(roleStr)) {
            roles = authorizationConfigService.get(username)
                    .getRoles();
            distributedCacheComponent.put(roleKey, JSONUtil.toJsonStr(roles));
        }else {
            List<RoleDto> roleDtos = JSONUtil.toList(JSONUtil.parseArray(roleStr), RoleDto.class);
            roles = roleDtos.stream().map(RoleData::new).collect(Collectors.toSet());
        }

        Set<Role> roleSet = new HashSet<>();
        return roles.stream().map(r -> {
            if (SystemRole.ADMIN.name().equals(r.getName())) {
                roleSet.add(new RuntimeRole(r, (a) -> new AdminAccessModel()));
            }else if (SystemRole.ANONYMOUS.name().equals(r.getName())) {
                roleSet.add(new RuntimeRole(r, AnonymousAccessModel::new));
            }else {
                roleSet.add(new RuntimeRole(r, AuthenticatedAccessModel::new));
            }
            return roleSet;
        }).flatMap(Collection::stream).collect(Collectors.toSet());

    }

}
