package com.veadan.folib.services.impl;

import com.github.pagehelper.PageHelper;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.entity.AccessToken;
import com.veadan.folib.forms.accesstoken.AccessTokenForm;
import com.veadan.folib.forms.accesstoken.AccessTokenResponse;
import com.veadan.folib.mapper.AccessTokenMapper;
import com.veadan.folib.scanner.common.util.UUIDUtils;
import com.veadan.folib.services.AccessTokenService;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.security.SecurityTokenProvider;
import com.veadan.folib.users.service.AccessTokenFinder;
import com.veadan.folib.users.userdetails.SpringSecurityUser;
import com.veadan.folib.utils.UserUtils;
import org.jose4j.lang.JoseException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import tk.mybatis.mapper.entity.Example;

import javax.annotation.Resource;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author huayanjun
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
        AccessToken accessToken = accessTokenMapper.selectByPrimaryKey(id);
        Assert.notNull(accessToken, "访问令牌不存在");
        Assert.isTrue(tokenId.equals(accessToken.getTokenId()), "无效的访问令牌");
        accessTokenMapper.deleteByPrimaryKey(id);
        distributedCacheComponent.delete(CACHE_KEY + tokenId);
    }

    @Override
    public List<AccessToken> list(Integer pageSize, Integer pageNum, String tokenId) {

        Example example = Example.builder(AccessToken.class).build();
        Example.Criteria where = example.createCriteria();
        if (StringUtils.hasText(tokenId)) {
            where.andEqualTo("tokenId", tokenId);
        } else {
            PageHelper.startPage(pageNum, pageSize);
        }
        example.setOrderByClause("create_time DESC");
        return accessTokenMapper.selectByExample(example);
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
            Example example = Example.builder(AccessToken.class).build();
            Example.Criteria where = example.createCriteria();
            where.andEqualTo("tokenId", jwtId);
            AccessToken accessToken = accessTokenMapper.selectOneByExample(example);
            if (accessToken == null) {
                return false;
            } else {
                distributedCacheComponent.put(CACHE_KEY + jwtId, CACHE_KEY);
                return true;
            }
        }
    }
}
