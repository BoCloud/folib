package com.veadan.folib.authentication.support;

/**
 * @author Przemyslaw Fusik
 */
public class ExternalRoleMapping
{

    private String externalRole;

    private String strongboxRole;

    public ExternalRoleMapping()
    {
        super();
    }

    public ExternalRoleMapping(String externalRole,
                               String strongboxRole)
    {
        super();
        this.externalRole = externalRole;
        this.strongboxRole = strongboxRole;
    }

    public String getExternalRole()
    {
        return externalRole;
    }

    public void setExternalRole(final String ldapRole)
    {
        this.externalRole = ldapRole;
    }

    public String getStrongboxRole()
    {
        return strongboxRole;
    }

    public void setStrongboxRole(final String strongboxRole)
    {
        this.strongboxRole = strongboxRole;
    }
}
