package com.veadan.folib.authentication.api.ldap;

import com.veadan.folib.authentication.support.ExternalRolesMappingFactoryBean;

public class LdapRolesMappingFactoryBean extends ExternalRolesMappingFactoryBean
{

    public LdapRolesMappingFactoryBean()
    {
        super("ldap");
    }
    
}
