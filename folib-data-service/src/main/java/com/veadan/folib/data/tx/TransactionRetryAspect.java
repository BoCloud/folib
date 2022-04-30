package com.veadan.folib.data.tx;

import java.util.stream.Stream;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.janusgraph.diskstorage.locking.PermanentLockingException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.interceptor.TransactionInterceptor;

/**
 * @author sbespalov
 *
 */
@Aspect
@Component
@Order(TransactionRetryAspect.ORDER)
public class TransactionRetryAspect
{

    public static final int ORDER = 95;

    private static final Logger logger = LoggerFactory.getLogger(TransactionRetryAspect.class);

    private static final ThreadLocal<Boolean> cflowbelow = new ThreadLocal<>();

    /**
     * This method will wrap "Top level" transactional invocations according to
     * rules defined with pointcut expressions.
     * "Top level" means the methods, declared with {@link Transactional}, where
     * the new Transaction has started and
     * will be ended after method invocation complete.
     * The goal is to retry intercepted (target) method in case of
     * {@link PermanentLockingException}.
     * The order of calling the interceptor is important, for this reason the
     * class is declared with {@link Order} annotation.
     * The order must be after the {@link TransactionInterceptor}.
     * 
     * @param jp
     * @return
     * @throws Throwable
     */
    @Around("execution(@org.springframework.transaction.annotation.Transactional * *(..)) " +
            "|| (execution(public * ((@org.springframework.transaction.annotation.Transactional *)+).*(..)) " +
            "&& within(@org.springframework.transaction.annotation.Transactional *)) ")
    public Object transactionalSpring(ProceedingJoinPoint jp)
        throws Throwable
    {
        return proceed(jp);
    }

    @Around("execution(@javax.transaction.Transactional * *(..)) " +
            "|| (execution(public * ((@javax.transaction.Transactional *)+).*(..)) " +
            "&& within(@javax.transaction.Transactional *))")
    public Object transactionalJta(ProceedingJoinPoint jp)
        throws Throwable
    {
        return proceed(jp);
    }

    private Object proceed(ProceedingJoinPoint jp)
        throws Throwable
    {
        if (Boolean.TRUE.equals(cflowbelow.get()))
        {
            return jp.proceed();
        }

        cflowbelow.set(Boolean.TRUE);
        logger.debug("Transactional method execution start.");

        try
        {
            return proceedWithRetry(jp);
        }
        finally
        {
            cflowbelow.remove();
        }
    }

    private Object proceedWithRetry(ProceedingJoinPoint jp)
        throws Throwable
    {
        RuntimeException lastException = null;
        for (int i = 0; i < 5; i++)
        {
            try
            {
                Object result = jp.proceed();
                logger.debug("Transactional method execution end.");
                return result;
            }
            catch (RuntimeException e)
            {
                lastException = e;
                Throwable rootCause = Stream.iterate(e, Throwable::getCause)
                                            .filter(element -> element.getCause() == null)
                                            .findFirst()
                                            .orElse(e);
                if (shouldRetry(rootCause))
                {
                    logger.warn(String.format("Retry [%s]-[%s].", i, jp.getSignature()));
                    Thread.sleep(100);
                    continue;
                }

                logger.debug("Transactional method execution end.");
                throw e;
            }
        }
        logger.debug("Transactional method execution end.");
        throw lastException;
    }

    private boolean shouldRetry(Throwable exception)
    {
        if (exception instanceof PermanentLockingException)
        {
            return true;
        }
        if (!(exception instanceof IllegalStateException))
        {
            return false;
        }

        IllegalStateException ise = (IllegalStateException) exception;
        if (ise.getMessage() == null)
        {
            return false;
        }

        return ise.getMessage().matches("Vertex with id \\d+ was removed.");
    }

}
