package com.veadan.folib.authentication.support;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.authority.mapping.GrantedAuthoritiesMapper;
import org.springframework.util.Assert;

import com.google.common.collect.ImmutableMap;

/**
 * @author veadan
 */
public class AuthoritiesExternalToInternalMapper
        implements InitializingBean,
                   GrantedAuthoritiesMapper
{

    private static final Logger logger = LoggerFactory.getLogger(AuthoritiesExternalToInternalMapper.class);

    private Map<String, String> rolesMapping;

    @Override
    public Collection<? extends GrantedAuthority> mapAuthorities(Collection<? extends GrantedAuthority> externalAuthorities)
    {
        logger.info("Map authorities [{}]", externalAuthorities);
        Collection<? extends GrantedAuthority> result = externalAuthorities.stream()
                                                                           .map(GrantedAuthority::getAuthority)
                                                                           .map(a -> getRolesMapping().get(a))
                                                                           .filter(Objects::nonNull)
                                                                           .map(SimpleGrantedAuthority::new)
                                                                           .collect(Collectors.toSet());
        logger.info("Authorities mapped [{}]", result);
        return result;
    }

    @Override
    public void afterPropertiesSet()
    {
        Assert.notEmpty(rolesMapping, "rolesMapping property not set");
    }

    /**
     * @return Returns an immutable map containing the same entries as {@link #rolesMapping}
     */
    public Map<String, String> getRolesMapping()
    {
        return rolesMapping;
    }

    public void setRolesMapping(Map<String, String> rolesMapping)
    {
        Assert.notNull(rolesMapping, "rolesMapping cannot be null");
        this.rolesMapping = ImmutableMap.copyOf(rolesMapping);
    }

}
