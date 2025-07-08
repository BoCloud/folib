package com.folib.forms.users;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.folib.validation.users.Password;

import java.io.Serializable;

@JsonIgnoreProperties(ignoreUnknown = true)
public class PasswordEncodeForm
        implements Serializable
{

    @Password(min = 8)
    @JsonProperty("password")
    private String password;

    @JsonCreator
    public PasswordEncodeForm(@JsonProperty("password") String password)
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
