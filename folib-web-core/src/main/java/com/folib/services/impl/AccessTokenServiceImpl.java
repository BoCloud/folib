/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.PageHelper;
import com.folib.components.DistributedCacheComponent;
import com.folib.entity.AccessToken;
import com.folib.forms.accesstoken.AccessTokenForm;
import com.folib.forms.accesstoken.AccessTokenResponse;
import com.folib.mapper.AccessTokenMapper;
import com.folib.scanner.common.util.UUIDUtils;
import com.folib.services.AccessTokenService;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.service.AccessTokenFinder;
import com.folib.users.userdetails.SpringSecurityUser;
import com.folib.utils.UserUtils;
import org.jose4j.lang.JoseException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @since 2024-08-20 13:49
 */
@Service
public class AccessTokenServiceImpl implements AccessTokenService, AccessTokenFinder {


    private final String CACHE_KEY = "folib:accessToke:";

    @Resource
    private AccessTokenMapper accessTokenMapper;

    @Resource
    private JwtClaimsProvider jwtClaimsProvider;

    @Resource
    private SecurityTokenProvider securityTokenProvider;

    @Resource
    private UserDetailsService userDetailsService;

    @Resource
    private DistributedCacheComponent distributedCacheComponent;

    @Override
    public AccessTokenResponse generate(AccessTokenForm form) {
        UserDetails userDetails = userDetailsService.loadUserByUsername(form.getUsername());
        Assert.notNull(userDetails, "用户不存在");
        Assert.isTrue(userDetails.isEnabled(), "用户不可用");
        Map<String, String> origin = jwtClaimsProvider.getClaims((SpringSecurityUser) userDetails);
        Map<String, String> claims = new HashMap<>(origin);
        claims.put("pac", "1");
        LocalDateTime expirationTime = getExpirationTime(form.getExpire());
        Long end = expirationTime == null ? null : expirationTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        String token;
        String uid = UUIDUtils.generateUuid();
        try {
            token = securityTokenProvider.getAccessToken(form.getUsername(), claims, end, uid);
        } catch (JoseException e) {
            throw new RuntimeException("生成token失败");
        }
        AccessToken tokenDO = new AccessToken();
        tokenDO.setTokenId(uid);
        tokenDO.setUsername(form.getUsername());
        tokenDO.setDescription(form.getDescription());
        tokenDO.setCreateTime(new Date());
        tokenDO.setOperator(UserUtils.getUsername());
        Date exp = expirationTime == null ? null : Date.from(expirationTime.atZone(ZoneId.systemDefault()).toInstant());
        tokenDO.setExpireTime(exp);
        accessTokenMapper.insert(tokenDO);
        AccessTokenResponse response = new AccessTokenResponse();
        response.setExp(exp);
        response.setJwt(token);
        response.setTokenId(uid);
        response.setUserName(form.getUsername());
        return response;
    }

    @Override
    public void delete(Long id, String tokenId) {
        AccessToken accessToken = accessTokenMapper.selectById(id);
        Assert.notNull(accessToken, "访问令牌不存在");
        Assert.isTrue(tokenId.equals(accessToken.getTokenId()), "无效的访问令牌");
        accessTokenMapper.deleteById(id);
        distributedCacheComponent.delete(CACHE_KEY + tokenId);
    }

    @Override
    public List<AccessToken> list(Integer pageSize, Integer pageNum, String tokenId) {
        if (!StringUtils.hasText(tokenId)) {
            PageHelper.startPage(pageNum, pageSize);
        }

       return accessTokenMapper.selectList(Wrappers.<AccessToken>lambdaQuery()
                .eq(StringUtils.hasText(tokenId),AccessToken::getTokenId, tokenId)
                .orderByDesc(AccessToken::getCreateTime)
        );
    }


    //0-不过期 1-7天 2-30天 3-90天 4-1年
    private LocalDateTime getExpirationTime(Integer expireType) {
        switch (expireType) {
            case 0:
                return null;
            case 1:
                return LocalDateTime.now().plusDays(7);
            case 2:
                return LocalDateTime.now().plusDays(30);
            case 3:
                return LocalDateTime.now().plusDays(90);
            case 4:
                return LocalDateTime.now().plusDays(365);
        }
        throw new IllegalArgumentException("无效的过期日期");
    }

    @Override
    public boolean getByJwtId(String jwtId) {
        if (CACHE_KEY.equals(distributedCacheComponent.get(CACHE_KEY + jwtId))) {
            return true;
        } else {
            AccessToken accessToken = accessTokenMapper.selectOne(Wrappers.<AccessToken>lambdaQuery().eq(AccessToken::getTokenId,jwtId));
            if (accessToken == null) {
                return false;
            } else {
                distributedCacheComponent.put(CACHE_KEY + jwtId, CACHE_KEY);
                return true;
            }
        }
    }
}
