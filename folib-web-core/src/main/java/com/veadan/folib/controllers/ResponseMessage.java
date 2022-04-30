package com.veadan.folib.controllers;

public class ResponseMessage
{

    private String message;

    public String getMessage()
    {
        return message;
    }

    public void setMessage(String message)
    {
        this.message = message;
    }

    public ResponseMessage withMessage(String message)
    {
        this.message = message;

        return this;
    }

    public static ResponseMessage empty()
    {
        return new ResponseMessage().withMessage("");
    }

    public static ResponseMessage ok()
    {
        return new ResponseMessage().withMessage("ok");
    }

}
