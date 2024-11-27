package com.veadan.folib.users.service;

/**
 * @author huayanjun
 * @since 2024-08-20 22:06
 */
public interface AccessTokenFinder {

    boolean getByJwtId(String jwtId);
}
