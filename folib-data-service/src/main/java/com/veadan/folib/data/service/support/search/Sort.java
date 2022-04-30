package com.veadan.folib.data.service.support.search;

import javax.annotation.concurrent.Immutable;
import java.util.Arrays;
import java.util.List;

import org.springframework.util.StringUtils;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class Sort
{

    public static final Direction DEFAULT_DIRECTION = Direction.ASC;

    private static final Sort DEFAULT_BY_UUID = Sort.by(Order.asc("uuid"));

    private final List<Order> orders;

    private Sort(List<Order> orders)
    {
        this.orders = orders;
    }

    public static Sort byUuid()
    {
        return DEFAULT_BY_UUID;
    }

    public static Sort by(List<Order> orders)
    {
        return new Sort(orders);
    }

    public static Sort by(Order... orders)
    {
        return new Sort(Arrays.asList(orders));
    }

    @Override
    public String toString()
    {
        return StringUtils.collectionToCommaDelimitedString(orders);
    }

    public enum Direction
    {

        ASC, DESC;

    }

    public static class Order
    {

        private final Direction direction;
        private final String property;

        private Order(Direction direction,
                      String property)
        {
            this.direction = direction == null ? DEFAULT_DIRECTION : direction;
            this.property = property;
        }

        public static Order by(String property)
        {
            return new Order(DEFAULT_DIRECTION, property);
        }

        public static Order asc(String property)
        {
            return new Order(Direction.ASC, property);
        }

        public static Order desc(String property)
        {
            return new Order(Direction.DESC, property);
        }

        @Override
        public String toString()
        {
            return String.format("%s %s", property, direction);
        }
    }
}
