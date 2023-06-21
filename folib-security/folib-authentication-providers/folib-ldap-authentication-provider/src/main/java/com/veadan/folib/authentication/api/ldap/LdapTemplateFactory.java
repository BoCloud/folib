package com.veadan.folib.authentication.api.ldap;

/**
 * @author leipenghui
 * @date 2023/6/19
 **/

import lombok.extern.slf4j.Slf4j;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.LdapContextSource;

/**
 * @author leipenhui
 * 初始化LDAP连接
 */
@Slf4j
public class LdapTemplateFactory {

    private static LdapTemplate LDAP_INSTANCE_TEMPLATE;

    public synchronized static LdapTemplate createLdapInstance(String url, String baseDN, String userDN, String password) {
        LDAP_INSTANCE_TEMPLATE = new LdapTemplate();
        LdapContextSource contextSource = new LdapContextSource();
        contextSource.setUrl(url);
        contextSource.setBase(baseDN);
        contextSource.setUserDn(userDN);
        contextSource.setPassword(password);
        contextSource.setReferral("follow");
        contextSource.setPooled(false);
        contextSource.afterPropertiesSet();
        LDAP_INSTANCE_TEMPLATE.setContextSource(contextSource);
        return LDAP_INSTANCE_TEMPLATE;
    }

}
