package com.veadan.folib.authentication.api.ldap;

import com.veadan.folib.authentication.api.AuthenticationItem;
import com.veadan.folib.authentication.api.AuthenticationItemConfigurationManager;
import com.veadan.folib.authentication.api.AuthenticationItems;
import com.veadan.folib.authentication.api.CustomAuthenticationItemMapper;
import com.veadan.folib.authentication.support.ExternalRoleMapping;

import javax.inject.Inject;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.ldap.core.LdapTemplate;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.ldap.userdetails.LdapUserDetailsService;
import org.springframework.stereotype.Component;

@Component
public class LdapAuthenticationConfigurationManager
        implements CustomAuthenticationItemMapper<LdapConfiguration>
{

    private static final String MANAGER_PASSWORD = "managerPassword";

    private static final String MANAGER_DN = "managerDn";

    public static final String AUTHENTICATION_ITEM_LDAP = "ldap";

    public static final String USER_DN_PATTERNS = "userDnPatterns";
    public static final String ROLES_MAPPING = "rolesMapping";
    public static final String CONVERT_TO_UPPER_CASE = "convertToUpperCase";
    public static final String USER_PASSWORD_ENCODED = "userPasswordEncoded";
    public static final String ROLE_PREFIX = "rolePrefix";
    public static final String GROUP_ROLE_ATTRIBUTE = "groupRoleAttribute";
    public static final String GROUP_SEARCH_FILTER = "groupSearchFilter";
    public static final String USER_SEARCH_FILTER = "userSearchFilter";
    public static final String SEARCH_SUBTREE = "searchSubtree";
    public static final String GROUP_SEARCH_BASE = "groupSearchBase";
    public static final String USER_SEARCH_BASE = "userSearchBase";
    public static final String URL = "url";
    public static final String AUTHORITIES = "authorities";

    @Inject
    private AuthenticationItemConfigurationManager authenticationItemConfigurationManager;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Override
    public String getConfigurationItemId()
    {
        return AUTHENTICATION_ITEM_LDAP;
    }

    public LdapConfiguration getConfiguration()
    {
        LdapConfiguration ldapConfiguration = authenticationItemConfigurationManager.getCustomAuthenticationItem(this);

        // TODO: This is a temporary solution to improve the user experience when enabling LDAP as a UserDetailsService.
        AuthenticationItems authenticationItems = authenticationItemConfigurationManager.getAuthenticationItems();
        List<AuthenticationItem> list = authenticationItems.getAuthenticationItemList();
        for (int i=0; i < list.size(); i++)
        {
            AuthenticationItem item = list.get(i);
            if(item.getName().equalsIgnoreCase("ldapUserDetailsService")) {
                ldapConfiguration.setEnableProvider(item.getEnabled());
            }
        }

        return ldapConfiguration;
    }

    public void updateConfiguration(LdapConfiguration configuration)
        throws IOException
    {
        authenticationItemConfigurationManager.putCustomAuthenticationItem(configuration, this);

        // TODO: This is a temporary solution to improve the user experience when enabling LDAP as a UserDetailsService.
        //       We should improve how this works with a later PR.
        AuthenticationItems authenticationItems = authenticationItemConfigurationManager.getAuthenticationItems();
        List<AuthenticationItem> list = authenticationItems.getAuthenticationItemList();
        for (int i = 0; i < list.size(); i++)
        {
            AuthenticationItem item = list.get(i);
            if (item.getName().equalsIgnoreCase("ldapUserDetailsService"))
            {
//                item.setOrder(configuration.getEnableProvider() ? 1 : 0);
                item.setEnabled(configuration.getEnableProvider());
            }
            else if(item.getName().equalsIgnoreCase("yamlUserDetailService"))
            {
//                item.setOrder(configuration.getEnableProvider() ? 0 : 1);
            }
        }

        authenticationItemConfigurationManager.updateAuthenticationItems(authenticationItems);
    }

    public void testConfiguration(String username,
                                  String password,
                                  LdapConfiguration configuration)
        throws IOException
    {
        authenticationItemConfigurationManager.testCustomAuthenticationItem(configuration, this, (c) -> {

            LdapUserDetailsService luds = (LdapUserDetailsService) c.getBean("ldapUserDetailsService");
            UserDetails user = luds.loadUserByUsername(username);
            if (!passwordEncoder.matches(password, user.getPassword()))
            {
                throw new BadCredentialsException("Credentials don't match.");
            }

            return false;
        });
    }

    public Map<String, Object> mapAuthorities(LdapAuthoritiesConfiguration source)
    {
        Map<String, Object> result = new HashMap<>();

        result.put(GROUP_SEARCH_BASE, source.getGroupSearch().getGroupSearchBase());
        result.put(GROUP_SEARCH_FILTER, source.getGroupSearch().getGroupSearchFilter());

        result.put(SEARCH_SUBTREE, source.isSearchSubtree());
        result.put(GROUP_ROLE_ATTRIBUTE, source.getGroupRoleAttribute());
        result.put(ROLE_PREFIX, source.getRolePrefix());
        result.put(CONVERT_TO_UPPER_CASE, source.isConvertToUpperCase());

        return result;
    }

    @Override
    public Map<String, Object> map(LdapConfiguration source)
    {
        Map<String, Object> result = new HashMap<>();

        result.put(URL, source.getUrl());
        result.put(MANAGER_DN, source.getManagerDn());
        result.put(MANAGER_PASSWORD, source.getManagerPassword());

        result.put(AUTHORITIES, mapAuthorities(source.getAuthoritiesConfiguration()));

        result.put(USER_SEARCH_BASE, source.getUserSearch().getUserSearchBase());
        result.put(USER_SEARCH_FILTER, source.getUserSearch().getUserSearchFilter());

        result.put(ROLES_MAPPING,
                   source.getRoleMappingList()
                         .stream()
                         .map(rm -> new HashMap<String, String>()
                         {
                             {
                                 put("externalRole", rm.getExternalRole());
                                 put("folibRole", rm.getFolibRole());
                             }
                         })
                         .collect(Collectors.toList()));

        result.put(USER_DN_PATTERNS, source.getUserDnPatternList());
        result.put(USER_PASSWORD_ENCODED, source.isUserPasswordEncoded());

        return result;
    }

    public LdapAuthoritiesConfiguration mapAuthorities(Map<String, Object> source)
    {
        LdapAuthoritiesConfiguration result = new LdapAuthoritiesConfiguration();

        result.setSearchSubtree(Boolean.TRUE.equals(source.get(SEARCH_SUBTREE)));
        result.setGroupRoleAttribute((String) source.get(GROUP_ROLE_ATTRIBUTE));
        result.setRolePrefix((String) source.get(ROLE_PREFIX));
        result.setConvertToUpperCase(Boolean.TRUE.equals(source.get(CONVERT_TO_UPPER_CASE)));

        LdapGroupSearch groupSearch = new LdapGroupSearch();
        groupSearch.setGroupSearchBase((String) source.get(GROUP_SEARCH_BASE));
        groupSearch.setGroupSearchFilter((String) source.get(GROUP_SEARCH_FILTER));
        result.setGroupSearch(groupSearch);

        return result;
    }

    @Override
    public LdapConfiguration map(Map<String, Object> source)
    {
        LdapConfiguration result = new LdapConfiguration();

        result.setUrl((String) source.get(URL));
        result.setManagerDn((String) source.get(MANAGER_DN));
        result.setManagerPassword(String.valueOf(source.get(MANAGER_PASSWORD)));
        result.setUserPasswordEncoded(Boolean.TRUE.equals(source.get(USER_PASSWORD_ENCODED)));

        LdapUserSearch userSearch = new LdapUserSearch();
        userSearch.setUserSearchBase((String) source.get(USER_SEARCH_BASE));
        userSearch.setUserSearchFilter((String) source.get(USER_SEARCH_FILTER));
        result.setUserSearch(userSearch);

        result.setAuthoritiesConfiguration(mapAuthorities((Map<String, Object>) source.get(AUTHORITIES)));

        result.setUserDnPatternList((List<String>) source.get(USER_DN_PATTERNS));

        result.setRoleMappingList(((List<Map<String, String>>) source.get(ROLES_MAPPING)).stream()
                                                                                         .map(rm -> new ExternalRoleMapping(
                                                                                                 rm.get("externalRole"),
                                                                                                 rm.get("folibRole")))
                                                                                         .collect(Collectors.toList()));

        return result;
    }

    /**
     * 初始化LDAP连接
     *
     * @return LdapTemplate
     */
    private LdapTemplate ldapTemplateInstance(LdapConfiguration ldap) {
        LdapTemplate ldapTemplate = LdapTemplateFactory.createLdapInstance(ldap.getUrl(),
                "", ldap.getManagerDn(), ldap.getManagerPassword());
        return ldapTemplate;
    }

    /**
     * 外部调用获取LdapTemplate
     * @return LdapTemplate
     */
    public LdapTemplate getLdapTemplateInstance() {
        return ldapTemplateInstance(getConfiguration());
    }

}
