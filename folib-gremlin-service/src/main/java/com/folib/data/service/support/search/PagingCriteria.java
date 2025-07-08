package com.folib.data.service.support.search;

import javax.annotation.Nonnull;
import javax.annotation.concurrent.Immutable;
import java.util.Objects;

/**
 * @author veadan
 */
@Immutable
public class PagingCriteria
{

    public static final PagingCriteria ALL = new PagingCriteria(0, -1);

    private final int skip;

    private final int limit;

    private final Sort sort;

    public PagingCriteria(final int skip,
                          final int limit,
                          @Nonnull final Sort sort)
    {
        Objects.requireNonNull(sort, "Sort cannot be null");

        this.skip = skip;
        this.limit = limit;
        this.sort = sort;
    }

    public PagingCriteria(final int skip,
                          final int limit)
    {
        this(skip, limit, Sort.byUuid());
    }

    public int getSkip()
    {
        return skip;
    }

    public int getLimit()
    {
        return limit;
    }

    @Nonnull
    public Sort getSort()
    {
        return sort;
    }
}
