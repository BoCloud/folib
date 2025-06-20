package com.veadan.folib.storage.indexing;

/**
 * @author mtodorov
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
