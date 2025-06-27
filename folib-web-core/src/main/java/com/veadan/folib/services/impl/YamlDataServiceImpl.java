package com.veadan.folib.services.impl;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.services.YamlDataService;
import com.veadan.folib.users.service.FolibRoleService;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author veadan
 * @Date: 2024/8/16 11:26
 * @Description:
 */
@Slf4j
@Service
public class YamlDataServiceImpl implements YamlDataService {

    @Autowired
    private StorageManagementService storageManagementService;
    @Autowired
    protected ConfigurationManagementService configurationManagementService;
    @Autowired
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;
    @Autowired
    private FolibRoleService folibRoleService;

    @Override
    public void syncYamlData() {
        //同步角色
        folibRoleService.syncYamlAuthorizationConfig();
        //同步存储空间用户
        storageManagementService.syncYamlStorageUsers(configurationManagementService.getConfiguration().getStorages().values());
        //同步用户
        ((RelationalDatabaseUserService) userService).syncUser();
;
    }
}
