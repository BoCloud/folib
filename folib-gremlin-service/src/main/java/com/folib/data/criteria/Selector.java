package com.folib.data.criteria;

import com.folib.data.domain.DomainObject;

/**
 * This class represent a final and ready to perform Query with target
 * projection and search expressions.
 * 
 * @author veadan
 *
 */
public class Selector<T extends DomainObject>
{

    private Class<T> targetClass;

    // TODO: we need something like ProjectionExpression here instead of just
    // String
    private String projection = "*";

    private Predicate predicate;

    private boolean fetch;
    
    private Paginator paginator = new Paginator();

    public Selector(Class<T> targetClass)
    {
        super();
        this.targetClass = targetClass;
    }

    public Class<T> getTargetClass()
    {
        return targetClass;
    }

    public Predicate getPredicate()
    {
        return predicate;
    }

    public String getProjection()
    {
        return projection;
    }

    public Selector<T> select(String projection)
    {
        this.projection = projection;
        return this;
    }

    public Predicate where(Expression e)
    {
        return this.predicate = Predicate.of(e);
    }

    public Predicate where(Predicate p)
    {
        return this.predicate = p;
    }

    public boolean isFetch()
    {
        return fetch;
    }

    public Selector<T> fetch()
    {
        this.fetch = true;
        return this;
    }

    public Paginator getPaginator()
    {
        return paginator;
    }

    public Selector<T> with(Paginator paginator)
    {
        this.paginator = paginator;
        return this;
    }

}
