package com.veadan.folib.authentication.api.ldap;

import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.NotEmpty;

import com.veadan.folib.authentication.support.ExternalRoleMapping;
import com.veadan.folib.validation.LdapUri;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonUnwrapped;

/**
 * @author veadan
 */
public class LdapConfiguration
{

    @LdapUri
    @NotEmpty
    private String url;

    private String managerDn;

    private String managerPassword;

    private boolean userPasswordEncoded;

    private LdapAuthoritiesConfiguration authoritiesConfiguration = new LdapAuthoritiesConfiguration();

    private LdapUserSearch userSearch = new LdapUserSearch();

    private List<ExternalRoleMapping> roleMappingList = new ArrayList<>();

    private List<String> userDnPatternList = new ArrayList<>();

    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private boolean enableProvider;

    public String getUrl()
    {
        return url;
    }

    public void setUrl(String url)
    {
        this.url = url;
    }

    public String getManagerDn()
    {
        return managerDn;
    }

    public void setManagerDn(String managerDn)
    {
        this.managerDn = managerDn;
    }

    public String getManagerPassword()
    {
        return managerPassword;
    }

    @JsonProperty("authorities")
    public LdapAuthoritiesConfiguration getAuthoritiesConfiguration()
    {
        return authoritiesConfiguration;
    }

    public void setAuthoritiesConfiguration(LdapAuthoritiesConfiguration authoritiesConfiguration)
    {
        this.authoritiesConfiguration = authoritiesConfiguration;
    }

    @JsonUnwrapped
    public LdapUserSearch getUserSearch()
    {
        return userSearch;
    }

    public void setUserSearch(LdapUserSearch userSearch)
    {
        this.userSearch = userSearch;
    }

    public void setManagerPassword(String managerPassword)
    {
        this.managerPassword = managerPassword;
    }

    public List<ExternalRoleMapping> getRoleMappingList()
    {
        return roleMappingList;
    }

    public void setRoleMappingList(List<ExternalRoleMapping> roleMappingList)
    {
        this.roleMappingList = roleMappingList;
    }

    public List<String> getUserDnPatternList()
    {
        return userDnPatternList;
    }

    public void setUserDnPatternList(List<String> userDnPatternList)
    {
        this.userDnPatternList = userDnPatternList;
    }

    public boolean getEnableProvider()
    {
        return enableProvider;
    }

    public void setEnableProvider(boolean enableProvider)
    {
        this.enableProvider = enableProvider;
    }

    public boolean isUserPasswordEncoded()
    {
        return userPasswordEncoded;
    }

    public void setUserPasswordEncoded(boolean userPasswordEncoded)
    {
        this.userPasswordEncoded = userPasswordEncoded;
    }
}
