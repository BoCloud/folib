package com.veadan.folib.util;

import java.time.Clock;
import java.time.Duration;
import java.time.LocalDateTime;

/**
 * @author carlspring
 */
public class LocalDateTimeInstance
{

    public static LocalDateTime now()
    {
        Clock microsecondClock = Clock.tick(Clock.systemUTC(), Duration.ofNanos(1000000));

        return LocalDateTime.now(microsecondClock);
    }

}
