package com.folib.authentication.api.nuget;

import java.util.Collections;
import java.util.Map;

import com.folib.users.domain.UserData;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.userdetails.SpringSecurityUser;

public class SecurityTokenClaimsProvider implements JwtClaimsProvider
{

    @Override
    public Map<String, String> getClaims(SpringSecurityUser user)
    {
        return Collections.singletonMap(UserData.SECURITY_TOKEN_KEY, user.getSecurityKey());
    }

}
