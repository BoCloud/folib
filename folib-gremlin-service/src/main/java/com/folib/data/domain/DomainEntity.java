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
package com.folib.data.domain;

import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.neo4j.ogm.annotation.Id;

/**
 * @author veadan
 *
 */
public class DomainEntity implements DomainObject
{

    private Long id;
    @Id
    private String uuid;

    @Override
    public Long getNativeId()
    {
        return id;
    }

    public void setNativeId(Long id)
    {
        this.id = id;
    }

    @Override
    public String getUuid()
    {
        return uuid;
    }

    public void setUuid(String uuid)
    {
        if (this.uuid != null && !this.uuid.equals(uuid))
        {
            throw new IllegalStateException(String.format("Can't change the uuid, [%s]->[%s].", this.uuid, uuid));
        }

        this.uuid = uuid;
    }

    @Override
    public void applyUnfold(Traverser<Vertex> t)
    {
        setNativeId((Long) t.get().id());
        setUuid((String) t.get().property("uuid").value());
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
        {
            return true;
        }
        else if (!(obj instanceof DomainEntity))
        {
            return false;
        }

        DomainEntity that = (DomainEntity) obj;
        if (this.uuid == null)
        {
            return false;
        }

        return this.uuid.equals(that.uuid);
    }

    @Override
    public int hashCode()
    {
        if (uuid == null)
        {
            return super.hashCode();
        }
        return uuid.hashCode();
    }

    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder(this.getClass().getSimpleName());
        sb.append("{");
        sb.append(", uuid='").append(uuid).append('\'');
        sb.append('}');
        return sb.toString();
    }

}
