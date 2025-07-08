package com.folib.services;

import com.folib.users.dto.UserAuthDTO;

/**
 * @author veadan
 */
public interface UserSyncService {

    void syncUserAuth(UserAuthDTO date);

}
