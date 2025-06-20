package com.veadan.folib.services;

import com.veadan.folib.entity.AccessToken;
import com.veadan.folib.dto.accesstoken.AccessTokenDto;
import com.veadan.folib.dto.accesstoken.AccessTokenResponse;

import java.util.List;

/**
 * @author huayanjun
 * @since 2024-08-20 13:49
 */
public interface AccessTokenService {

    AccessTokenResponse generate(AccessTokenDto form);

    void delete(Long id,String tokenId);

    List<AccessToken> list(Integer pageSize, Integer pageNum,String tokenId);


}
