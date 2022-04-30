package com.veadan.folib.users.security;

import java.io.IOException;
import java.util.Set;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.veadan.folib.authorization.AuthorizationConfigFileManager;
import com.veadan.folib.authorization.domain.RoleData;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.dto.Role;
import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.users.domain.SystemRole;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Przemyslaw Fusik
 */
@Component
public class AuthoritiesProvider
{

    private static final Logger logger = LoggerFactory.getLogger(AuthoritiesProvider.class);

    @Inject
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;

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
        RoleData role = authorizationConfigService.get()
                                                  .getRoles()
                                                  .stream()
                                                  .filter(r -> r.getName().equals(name))
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

}
