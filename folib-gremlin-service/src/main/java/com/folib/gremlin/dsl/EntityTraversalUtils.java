/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.gremlin.dsl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import com.folib.data.domain.DomainObject;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Property;
import org.folib.util.Commons;

/**
 * Utility methods to work with {@link EntityTraversalDsl} traversals.
 *
 * @author veadan
 */
public class EntityTraversalUtils
{
    public static final String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss.SSSXXX";

    public static <E> E extractObject(Class<E> target,
                                      Object value)
    {
        return Optional.ofNullable(value)
                       .filter(v -> !EntityTraversalDsl.NULL.equals(v))
                       .map(target::cast)
                       .orElse(null);
    }

    public static Date extractDate(Object value)
    {
        return Optional.ofNullable(value)
                       .filter(v -> !EntityTraversalDsl.NULL.equals(v))
                       .map(String.class::cast)
                       .map(EntityTraversalUtils::format)
                       .orElse(null);
    }

    protected static Date format(String t)
    {
        try
        {
            return new SimpleDateFormat(DATE_FORMAT).parse(t);
        }
        catch (ParseException e)
        {
            throw new IllegalArgumentException();
        }
    }

    public static <E> List<E> extractPropertyList(Class<E> target,
                                                  Object value)
    {
        return Optional.ofNullable(value)
                       .filter(v -> !EntityTraversalDsl.NULL.equals(v))
                       .map(v -> (List<Property<Object>>) v)
                       .<List<E>>map(c -> c.stream().map(p -> p.value()).map(target::cast).collect(Collectors.toList()))
                       .orElse(Collections.emptyList());
    }

    public static <E> Object castToObject(Traverser<E> t)
    {
        return Object.class.cast(t.get());
    }

    public static void traceVertex(Traverser<? extends Element> t)
    {
        System.out.println(String.format("[%s]-[%s]-[%s]",
                                         t.get().label(),
                                         t.get().id(),
                                         t.get().property("uuid").orElse("empty")));
    }

    public static <T extends DomainObject> List<T> reduceHierarchy(List<T> entityList)
    {
        Map<String, List<T>> resultMapByUuid = Optional.ofNullable(entityList).orElse(Collections.emptyList()).stream()
                                                         .collect(Collectors.groupingBy(DomainObject::getUuid,
                                                                                        Collectors.toCollection(LinkedList::new)));

        return resultMapByUuid.values()
                              .stream()
                              .map(e -> e.stream()
                                         .sorted((a1,
                                                  a2) -> a1.getClass().isInstance(a2) ? 1 : -1)
                                         .findFirst()
                                         .get())
                              .collect(Collectors.toList());

    }

    public static LocalDateTime toLocalDateTime(Date date)
    {
        return Commons.toLocalDateTime(date);
    }

    public static Date toDate(LocalDateTime date)
    {
        return Commons.toDate(date);
    }

    public static LocalDateTime toLocalDateTime(Long value)
    {
        return Commons.toLocalDateTime(value);
    }

    public static Long toLong(LocalDateTime date)
    {
        return Commons.toLong(date);
    }

    static <E2> void created(Traverser<E2> t)
    {
        info("Created", t);
    }

    static <E2> void info(String action,
                          Traverser<E2> t)
    {
        EntityTraversalDsl.logger.info(String.format("%s [%s]-[%s]-[%s]",
                                                     action,
                                                     ((Element) t.get()).label(),
                                                     ((Element) t.get()).id(),
                                                     ((Element) t.get()).property("uuid")
                                                                        .orElse("null")));
    }

    static <E2> void debug(String action,
                           Traverser<E2> t)
    {
        EntityTraversalDsl.logger.debug(String.format("%s [%s]-[%s]-[%s]",
                                                      action,
                                                      ((Element) t.get()).label(),
                                                      ((Element) t.get()).id(),
                                                      ((Element) t.get()).property("uuid")
                                                                         .orElse("null")));
    }

    static <E2> void fetched(Traverser<E2> t)
    {
        debug("Fetched", t);
    }
}
