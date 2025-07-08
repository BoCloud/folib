package com.folib.event.privilege;

import com.folib.event.Event;

/**
 * @author veadan
 */
public class PrivilegeEvent extends Event
{

    private String uuId;

    public PrivilegeEvent(String uuId,
                          int type)
    {
        super(type);
        this.uuId = uuId;
    }

    public String getUuId() {
        return uuId;
    }

    public void setUuId(String uuId) {
        this.uuId = uuId;
    }

}
