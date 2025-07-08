package com.folib.npm;

public class NpmSearchRequest
{

    private String text;

    private Integer size;

    public String getText()
    {
        return text;
    }

    public void setText(String packageId)
    {
        this.text = packageId;
    }

    public Integer getSize()
    {
        return size;
    }

    public void setSize(Integer size)
    {
        this.size = size;
    }

}
