package com.veadan.folib.services;

import com.veadan.folib.entity.AccessToken;
import com.veadan.folib.forms.accesstoken.AccessTokenForm;
import com.veadan.folib.forms.accesstoken.AccessTokenResponse;

import java.util.List;

/**
 * @author veadan
 * @since 2024-08-20 13:49
 */
public interface AccessTokenService {

    AccessTokenResponse generate(AccessTokenForm form);

    void delete(Long id,String tokenId);

    List<AccessToken> list(Integer pageSize, Integer pageNum,String tokenId);


}
