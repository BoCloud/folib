package com.folib.authentication.api;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;

public interface AuthenticationCache extends PasswordEncoder
{

    public UsernamePasswordAuthenticationToken getAuthenticationToken(String userName);

    public UsernamePasswordAuthenticationToken putAuthenticationToken(UsernamePasswordAuthenticationToken authentication);

}
