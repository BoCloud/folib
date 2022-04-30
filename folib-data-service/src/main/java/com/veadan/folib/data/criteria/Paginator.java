package com.veadan.folib.data.criteria;

public class Paginator
{

    public static final Integer MAX_LIMIT = 1000;

    private Long skip;
    private Integer limit;

    private String property;
    private Order order = Order.ASC;

    public Long getSkip()
    {
        return skip == null ? Integer.valueOf(0) : skip;
    }

    public void setSkip(Long skip)
    {
        this.skip = skip;
    }

    public Integer getLimit()
    {
        return limit == null || limit < 0 || limit > MAX_LIMIT ? MAX_LIMIT : limit;
    }

    public void setLimit(Integer limit)
    {
        this.limit = limit;
    }

    public String getProperty()
    {
        return property;
    }

    public void setProperty(String orderBy)
    {
        this.property = orderBy;
    }

    public Order getOrder()
    {
        return order;
    }

    public void setOrder(Order order)
    {
        this.order = order;
    }

    public static enum Order
    {
        ASC, DESC;
    }
}
