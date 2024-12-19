package com.veadan.folib.components;

import com.veadan.folib.users.domain.Privileges;

import java.util.Collection;
import java.util.List;

/**
 * @author huayanjun
 * @since 2024-12-19 14:14
 */
public interface CostumeSecurityAdapter {

    Collection<Privileges> getStorageAuthorities(String storageId, String repositoryId, List<String> paths);
}
