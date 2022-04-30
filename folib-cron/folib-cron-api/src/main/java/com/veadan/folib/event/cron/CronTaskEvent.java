package com.veadan.folib.event.cron;

import com.veadan.folib.event.Event;

/**
 * @author mtodorov
 */
public class CronTaskEvent
        extends Event
{

    private String name;


    public CronTaskEvent(int type, String name)
    {
        super(type);
        setName(name);
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

}
