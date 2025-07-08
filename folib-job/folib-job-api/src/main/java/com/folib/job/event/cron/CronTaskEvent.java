package com.folib.job.event.cron;

import com.folib.event.Event;

/**
 * @author veadan
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
