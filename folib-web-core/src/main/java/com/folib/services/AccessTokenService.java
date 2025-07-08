package com.folib.services;

import com.folib.entity.AccessToken;
import com.folib.forms.accesstoken.AccessTokenForm;
import com.folib.forms.accesstoken.AccessTokenResponse;

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
