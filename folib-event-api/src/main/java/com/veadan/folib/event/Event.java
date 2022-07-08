package com.veadan.folib.event;

import org.springframework.context.ApplicationEvent;

/**
 * @author Veadan
 */
public class Event extends ApplicationEvent
{

    public Event(Object source)
    {
        super(source);
    }


    public int getType()
    {
        return (int) getSource();
    }

}
