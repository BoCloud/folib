package com.veadan.folib.event.validator;

public enum ValidatorEventTypeEnum {

    /**
     * 存储校验器
     */
    STORAGE_VALIDATOR(1);

    private int type;


    ValidatorEventTypeEnum(int type)
    {
        this.type = type;
    }

    public int getType()
    {
        return type;
    }
}
