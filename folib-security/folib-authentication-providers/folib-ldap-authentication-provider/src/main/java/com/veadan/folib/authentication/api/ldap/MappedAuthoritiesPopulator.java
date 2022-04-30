package com.veadan.folib.authentication.api.ldap;

import java.util.Collection;

import org.springframework.ldap.core.DirContextOperations;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.mapping.GrantedAuthoritiesMapper;
import org.springframework.security.ldap.authentication.NullLdapAuthoritiesPopulator;
import org.springframework.security.ldap.userdetails.LdapAuthoritiesPopulator;

/**
 * @author sbespalov
 *
 */
public class MappedAuthoritiesPopulator implements LdapAuthoritiesPopulator
{

    private final GrantedAuthoritiesMapper authoritiesMapper;

    private LdapAuthoritiesPopulator delegate = new NullLdapAuthoritiesPopulator();

    public MappedAuthoritiesPopulator(GrantedAuthoritiesMapper authoritiesMapper)
    {
        super();
        this.authoritiesMapper = authoritiesMapper;
    }

    public LdapAuthoritiesPopulator getDelegate()
    {
        return delegate;
    }

    public void setDelegate(LdapAuthoritiesPopulator delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Collection<? extends GrantedAuthority> getGrantedAuthorities(DirContextOperations userData,
                                                                        String username)
    {
        return authoritiesMapper.mapAuthorities(getDelegate().getGrantedAuthorities(userData, username));
    }

}
