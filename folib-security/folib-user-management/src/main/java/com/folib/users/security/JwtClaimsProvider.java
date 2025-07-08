package com.folib.users.security;

import java.util.Map;

import com.folib.users.userdetails.SpringSecurityUser;

public interface JwtClaimsProvider
{

    Map<String, String> getClaims(SpringSecurityUser user);
    
}
