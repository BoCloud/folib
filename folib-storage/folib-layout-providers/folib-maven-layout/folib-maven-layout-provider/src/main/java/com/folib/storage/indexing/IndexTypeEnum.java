package com.folib.storage.indexing;

/**
 * @author veadan
 */
public enum IndexTypeEnum
{

    LOCAL("local"),

    REMOTE("remote");


    private String type;


    IndexTypeEnum(String type)
    {
        this.type = type;
    }

    public String getType()
    {
        return type;
    }

}
