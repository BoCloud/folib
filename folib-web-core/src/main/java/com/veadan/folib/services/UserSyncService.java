package com.veadan.folib.services;

import com.veadan.folib.users.dto.UserAuthDTO;

/**
 * @author xuxinping
 */
public interface UserSyncService {

    void syncUserAuth(UserAuthDTO date);

}
