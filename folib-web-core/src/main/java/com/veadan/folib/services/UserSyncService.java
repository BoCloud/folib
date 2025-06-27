package com.veadan.folib.services;

import com.veadan.folib.users.dto.UserAuthDTO;

/**
 * @author veadan
 */
public interface UserSyncService {

    void syncUserAuth(UserAuthDTO date);

}
