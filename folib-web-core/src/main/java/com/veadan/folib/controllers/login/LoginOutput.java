package com.veadan.folib.controllers.login;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.stream.Collectors;

import org.springframework.security.core.GrantedAuthority;

/**
 * @author Przemyslaw Fusik
 */
@XmlRootElement(name = "login-output")
@XmlAccessorType(XmlAccessType.NONE)
public class LoginOutput
{

    @XmlElement
    private String token;

    @XmlElement()
    private LinkedHashSet<String> authorities;


    public LoginOutput()
    {
    }

    public LoginOutput(String token,
                       Collection<? extends GrantedAuthority> authorities)
    {
        this.token = token;
        this.authorities = authorities.stream()
                                      .map(GrantedAuthority::getAuthority)
                                      .sorted()
                                      .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    public String getToken()
    {
        return token;
    }

    public void setToken(String token)
    {
        this.token = token;
    }

    public LinkedHashSet<String> getAuthorities()
    {
        return authorities;
    }

    public void setAuthorities(LinkedHashSet<String> authorities)
    {
        this.authorities = authorities;
    }

}
