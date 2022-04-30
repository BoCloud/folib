package com.veadan.folib.security.certificates;

import java.net.Authenticator;
import java.net.PasswordAuthentication;

import org.springframework.stereotype.Component;

/**
 * @author carlspring
 */
@Component
public class ProxyAuthenticator
        extends Authenticator
{

    final ThreadLocal<PasswordAuthentication> credentials = new ThreadLocal<>();

    @Override
    protected PasswordAuthentication getPasswordAuthentication()
    {
        return credentials.get();
    }

    public ThreadLocal<PasswordAuthentication> getCredentials()
    {
        return credentials;
    }

}
