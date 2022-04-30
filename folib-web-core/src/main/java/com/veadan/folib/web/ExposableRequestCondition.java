package com.veadan.folib.web;

import javax.servlet.http.HttpServletRequest;

import org.springframework.web.servlet.mvc.condition.AbstractRequestCondition;

/**
 * @author Przemyslaw Fusik
 */
public abstract class ExposableRequestCondition
        extends AbstractRequestCondition<ExposableRequestCondition>
{

    protected abstract void expose(HttpServletRequest request);

    @Override
    public ExposableRequestCondition combine(ExposableRequestCondition other)
    {
        return this;
    }

    @Override
    public ExposableRequestCondition getMatchingCondition(HttpServletRequest request)
    {
        return this;
    }

    @Override
    public int compareTo(ExposableRequestCondition other,
                         HttpServletRequest request)
    {
        return 1;
    }
}
