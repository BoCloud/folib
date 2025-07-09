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
package com.folib.repositories;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.neo4j.ogm.cypher.Filter;
import org.neo4j.ogm.cypher.Filters;
import org.neo4j.ogm.cypher.query.Pagination;
import org.neo4j.ogm.cypher.query.SortOrder;
import org.neo4j.ogm.model.Result;
import org.neo4j.ogm.session.LoadStrategy;
import org.neo4j.ogm.session.Session;
import org.neo4j.ogm.session.event.Event;
import org.neo4j.ogm.session.event.EventListener;
import org.neo4j.ogm.transaction.Transaction;
import org.neo4j.ogm.transaction.Transaction.Type;

public class IdBlockQueueSession implements Session
{

    private final Session target;
    private final String idBlockQueueName;

    public IdBlockQueueSession(String idBlockQueueName, Session target)
    {
        this.target = target;
        this.idBlockQueueName = idBlockQueueName;
    }

    public String getIdBlockQueueName()
    {
        return idBlockQueueName;
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids)
    {
        return target.loadAll(type, ids);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              int depth)
    {
        return target.loadAll(type, ids, depth);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              SortOrder sortOrder)
    {
        return target.loadAll(type, ids, sortOrder);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              SortOrder sortOrder,
                                                              int depth)
    {
        return target.loadAll(type, ids, sortOrder, depth);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              Pagination pagination)
    {
        return target.loadAll(type, ids, pagination);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              Pagination pagination,
                                                              int depth)
    {
        return target.loadAll(type, ids, pagination, depth);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              SortOrder sortOrder,
                                                              Pagination pagination)
    {
        return target.loadAll(type, ids, sortOrder, pagination);
    }

    public <T, ID extends Serializable> Collection<T> loadAll(Class<T> type,
                                                              Collection<ID> ids,
                                                              SortOrder sortOrder,
                                                              Pagination pagination,
                                                              int depth)
    {
        return target.loadAll(type, ids, sortOrder, pagination, depth);
    }

    public <T> Collection<T> loadAll(Collection<T> objects)
    {
        return target.loadAll(objects);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     int depth)
    {
        return target.loadAll(objects, depth);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     SortOrder sortOrder)
    {
        return target.loadAll(objects, sortOrder);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     SortOrder sortOrder,
                                     int depth)
    {
        return target.loadAll(objects, sortOrder, depth);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     Pagination pagination)
    {
        return target.loadAll(objects, pagination);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(objects, pagination, depth);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     SortOrder sortOrder,
                                     Pagination pagination)
    {
        return target.loadAll(objects, sortOrder, pagination);
    }

    public <T> Collection<T> loadAll(Collection<T> objects,
                                     SortOrder sortOrder,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(objects, sortOrder, pagination, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type)
    {
        return target.loadAll(type);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     int depth)
    {
        return target.loadAll(type, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     SortOrder sortOrder)
    {
        return target.loadAll(type, sortOrder);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     SortOrder sortOrder,
                                     int depth)
    {
        return target.loadAll(type, sortOrder, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Pagination pagination)
    {
        return target.loadAll(type, pagination);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(type, pagination, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     SortOrder sortOrder,
                                     Pagination pagination)
    {
        return target.loadAll(type, sortOrder, pagination);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     SortOrder sortOrder,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(type, sortOrder, pagination, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter)
    {
        return target.loadAll(type, filter);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     int depth)
    {
        return target.loadAll(type, filter, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     SortOrder sortOrder)
    {
        return target.loadAll(type, filter, sortOrder);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     SortOrder sortOrder,
                                     int depth)
    {
        return target.loadAll(type, filter, sortOrder, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     Pagination pagination)
    {
        return target.loadAll(type, filter, pagination);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(type, filter, pagination, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     SortOrder sortOrder,
                                     Pagination pagination)
    {
        return target.loadAll(type, filter, sortOrder, pagination);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filter filter,
                                     SortOrder sortOrder,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(type, filter, sortOrder, pagination, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters)
    {
        return target.loadAll(type, filters);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     int depth)
    {
        return target.loadAll(type, filters, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     SortOrder sortOrder)
    {
        return target.loadAll(type, filters, sortOrder);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     SortOrder sortOrder,
                                     int depth)
    {
        return target.loadAll(type, filters, sortOrder, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     Pagination pagination)
    {
        return target.loadAll(type, filters, pagination);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(type, filters, pagination, depth);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     SortOrder sortOrder,
                                     Pagination pagination)
    {
        return target.loadAll(type, filters, sortOrder, pagination);
    }

    public <T> Collection<T> loadAll(Class<T> type,
                                     Filters filters,
                                     SortOrder sortOrder,
                                     Pagination pagination,
                                     int depth)
    {
        return target.loadAll(type, filters, sortOrder, pagination, depth);
    }

    public <T, ID extends Serializable> T load(Class<T> type,
                                               ID id)
    {
        return target.load(type, id);
    }

    public <T, ID extends Serializable> T load(Class<T> type,
                                               ID id,
                                               int depth)
    {
        return target.load(type, id, depth);
    }

    public <T> void save(T object)
    {
        target.save(object);
    }

    public <T> void save(T object,
                         int depth)
    {
        target.save(object, depth);
    }

    public <T> void delete(T object)
    {
        target.delete(object);
    }

    public <T> void deleteAll(Class<T> type)
    {
        target.deleteAll(type);
    }

    public <T> Object delete(Class<T> type,
                             Iterable<Filter> filters,
                             boolean listResults)
    {
        return target.delete(type, filters, listResults);
    }

    public void purgeDatabase()
    {
        target.purgeDatabase();
    }

    public void clear()
    {
        target.clear();
    }

    public Transaction getTransaction()
    {
        return target.getTransaction();
    }

    public Transaction beginTransaction()
    {
        return target.beginTransaction();
    }

    public Transaction beginTransaction(Type type)
    {
        return target.beginTransaction(type);
    }

    public Transaction beginTransaction(Type type,
                                        Iterable<String> bookmarks)
    {
        return target.beginTransaction(type, bookmarks);
    }

    public <T> T queryForObject(Class<T> objectType,
                                String cypher,
                                Map<String, ?> parameters)
    {
        return target.queryForObject(objectType, cypher, parameters);
    }

    public <T> Iterable<T> query(Class<T> objectType,
                                 String cypher,
                                 Map<String, ?> parameters)
    {
        return target.query(objectType, cypher, parameters);
    }

    public Result query(String cypher,
                        Map<String, ?> parameters)
    {
        return target.query(cypher, parameters);
    }


    public Result query(String cypher,
                        Map<String, ?> parameters,
                        boolean readOnly)
    {
        return target.query(cypher, parameters, readOnly);
    }

    public long countEntitiesOfType(Class<?> entity)
    {
        return target.countEntitiesOfType(entity);
    }

    public long count(Class<?> clazz,
                      Iterable<Filter> filters)
    {
        return target.count(clazz, filters);
    }

    public Long resolveGraphIdFor(Object possibleEntity)
    {
        return target.resolveGraphIdFor(possibleEntity);
    }

    public boolean detachNodeEntity(Long id)
    {
        return target.detachNodeEntity(id);
    }

    public boolean detachRelationshipEntity(Long id)
    {
        return target.detachRelationshipEntity(id);
    }

    public EventListener register(EventListener eventListener)
    {
        return target.register(eventListener);
    }

    public boolean dispose(EventListener eventListener)
    {
        return target.dispose(eventListener);
    }

    public void notifyListeners(Event event)
    {
        target.notifyListeners(event);
    }

    public boolean eventsEnabled()
    {
        return target.eventsEnabled();
    }

    public String getLastBookmark()
    {
        return target.getLastBookmark();
    }

    public void withBookmark(String bookmark)
    {
        target.withBookmark(bookmark);
    }

    public LoadStrategy getLoadStrategy()
    {
        return target.getLoadStrategy();
    }

    public void setLoadStrategy(LoadStrategy loadStrategy)
    {
        target.setLoadStrategy(loadStrategy);
    }

}
