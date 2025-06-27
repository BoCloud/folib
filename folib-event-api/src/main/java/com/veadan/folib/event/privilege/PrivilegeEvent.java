package com.veadan.folib.event.privilege;

import com.veadan.folib.event.Event;
import com.veadan.folib.event.RepositoryBasedEvent;

import java.nio.file.Path;

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
