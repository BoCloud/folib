package com.veadan.folib.dto.users;

import com.veadan.folib.validation.users.Password;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PasswordEncodeDto
        implements Serializable
{

    @Password(min = 8)
    @JsonProperty("password")
    private String password;

    @JsonCreator
    public PasswordEncodeDto(@JsonProperty("password") String password)
    {
        this.password = password;
    }

    public String getPassword()
    {
        return password;
    }

    public void setPassword(String password)
    {
        this.password = password;
    }

}
