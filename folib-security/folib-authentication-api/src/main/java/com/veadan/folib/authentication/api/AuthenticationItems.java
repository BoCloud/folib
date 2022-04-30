package com.veadan.folib.authentication.api;

import java.util.ArrayList;
import java.util.List;

public class AuthenticationItems
{

    private List<AuthenticationItem> authenticationItemList = new ArrayList<>();

    public List<AuthenticationItem> getAuthenticationItemList()
    {
        return authenticationItemList;
    }

    public void setAuthenticationItemList(List<AuthenticationItem> authenticationItemList)
    {
        this.authenticationItemList = authenticationItemList;
    }

}
