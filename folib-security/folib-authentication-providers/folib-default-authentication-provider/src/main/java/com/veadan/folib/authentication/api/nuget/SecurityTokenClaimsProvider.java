package com.veadan.folib.authentication.api.nuget;

import java.util.Collections;
import java.util.Map;

import com.veadan.folib.users.domain.UserData;
import com.veadan.folib.users.security.JwtClaimsProvider;
import com.veadan.folib.users.userdetails.SpringSecurityUser;

public class SecurityTokenClaimsProvider implements JwtClaimsProvider
{

    @Override
    public Map<String, String> getClaims(SpringSecurityUser user)
    {
        return Collections.singletonMap(UserData.SECURITY_TOKEN_KEY, user.getSecurityKey());
    }

}
