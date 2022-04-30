package com.veadan.folib.domain;

public enum RpmPackageType
{
    SOURCE("src"),
    BINARY("");

    private String postfix;

    public String getPostfix()
    {
        return postfix;
    }

    RpmPackageType(String postfix)
    {
        this.postfix = postfix;
    }
}
