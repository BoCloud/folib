package com.veadan.folib.users.security;

import java.util.Map;

import com.veadan.folib.users.userdetails.SpringSecurityUser;

public interface JwtClaimsProvider
{

    Map<String, String> getClaims(SpringSecurityUser user);
    
}
