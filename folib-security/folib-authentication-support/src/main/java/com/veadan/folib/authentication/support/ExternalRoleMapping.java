package com.veadan.folib.authentication.support;

/**
 * @author veadan
 */
public class ExternalRoleMapping
{

    private String externalRole;

    private String folibRole;

    public ExternalRoleMapping()
    {
        super();
    }

    public ExternalRoleMapping(String externalRole,
                               String folibRole)
    {
        super();
        this.externalRole = externalRole;
        this.folibRole = folibRole;
    }

    public String getExternalRole()
    {
        return externalRole;
    }

    public void setExternalRole(final String ldapRole)
    {
        this.externalRole = ldapRole;
    }

    public String getFolibRole()
    {
        return folibRole;
    }

    public void setFolibRole(final String folibRole)
    {
        this.folibRole = folibRole;
    }
}
