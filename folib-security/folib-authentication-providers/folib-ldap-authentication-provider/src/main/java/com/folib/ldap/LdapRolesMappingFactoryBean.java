package com.folib.ldap;

import com.folib.authentication.support.ExternalRolesMappingFactoryBean;

public class LdapRolesMappingFactoryBean extends ExternalRolesMappingFactoryBean
{

    public LdapRolesMappingFactoryBean()
    {
        super("ldap");
    }
    
}
