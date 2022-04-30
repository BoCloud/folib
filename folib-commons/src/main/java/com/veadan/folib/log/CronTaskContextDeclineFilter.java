package com.veadan.folib.log;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;

public class CronTaskContextDeclineFilter extends Filter<ILoggingEvent>
{
    private Filter<ILoggingEvent> target = new CronTaskContextAcceptFilter();

    @Override
    public FilterReply decide(ILoggingEvent event)
    {
        FilterReply filterReply = target.decide(event);
        if (FilterReply.NEUTRAL.equals(filterReply))
        {
            return FilterReply.DENY;
        }
        return FilterReply.NEUTRAL;
    }

}
