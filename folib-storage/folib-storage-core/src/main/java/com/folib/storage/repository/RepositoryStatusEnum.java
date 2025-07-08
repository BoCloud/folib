package com.folib.storage.repository;

import com.folib.api.Describable;

/**
 * @author veadan
 */
public enum RepositoryStatusEnum implements Describable
{

    IN_SERVICE("In Service"),

    OUT_OF_SERVICE("Out of Service");

    private String status;


    RepositoryStatusEnum(String status)
    {
        this.status = status;
    }

    public String getStatus()
    {
        return status;
    }

    @Override
    public String describe()
    {
        return getStatus();
    }

}
