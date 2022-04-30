package com.veadan.folib.log;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;

/**
 * 
 * @author ankit.tomar
 *
 */
public class JaninoClassLoaderLogDeclineFilter extends Filter<ILoggingEvent>
{

    @Override
    public FilterReply decide(ILoggingEvent event)
    {
        if (event.getLoggerName().contains("org.codehaus.janino"))
        {
            return FilterReply.DENY;
        }
        return FilterReply.NEUTRAL;
    }

}
