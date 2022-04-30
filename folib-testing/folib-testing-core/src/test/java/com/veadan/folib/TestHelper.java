package com.veadan.folib;

import java.util.function.Function;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.time.StopWatch;

/**
 * @author Przemyslaw Fusik
 */
public class TestHelper
{

    public static <I> boolean isOperationSuccessed(Function<I, Boolean> operation,
                                                   I argument,
                                                   int millisTimeout,
                                                   int millisSleepTime)
            throws InterruptedException
    {
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        boolean result = BooleanUtils.isTrue(operation.apply(argument));
        while (stopWatch.getTime() < millisTimeout && !result)
        {
            result = BooleanUtils.isTrue(operation.apply(argument));
            Thread.sleep(millisSleepTime);
        }
        stopWatch.stop();
        return result;
    }

}
