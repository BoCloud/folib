package com.folib.event.server;

/**
 * @author Veadan
 */
public enum ServerEventTypeEnum
{

    /**
     * Occurs when the server has started initializing.
     *
     * TODO: Not yet implemented.
     */
    EVENT_SERVER_INITIALIZING(1),

    /**
     * Occurs when the server has finished initializing and has fully started.
     *
     * TODO: Not yet implemented.
     */
    EVENT_SERVER_STARTED(2),

    /**
     * Occurs when the server has begun a graceful shutdown.
     *
     * TODO: Not yet implemented.
     */
    EVENT_SERVER_STOPPING(3),

    /**
     * Occurs when the server has stopped.
     *
     * TODO: Not yet implemented.
     */
    EVENT_SERVER_STOPPED(4),

    /**
     * Occurs when the server's configuration has been changed.
     *
     * TODO: Not yet implemented.
     */
    EVENT_SERVER_CONFIGURATION_CHANGED(5);

    private int type;


    ServerEventTypeEnum(int type)
    {
        this.type = type;
    }

    public int getType()
    {
        return type;
    }

}
