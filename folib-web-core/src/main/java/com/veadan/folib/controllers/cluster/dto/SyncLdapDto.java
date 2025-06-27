package com.veadan.folib.controllers.cluster.dto;

import com.veadan.folib.authentication.api.ldap.LdapConfiguration;
import com.veadan.folib.cluster.SyncLdapEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SyncLdapDto {

    private LdapConfiguration ldapConfiguration;

    private SyncLdapEnum syncLdapEnum;
}
