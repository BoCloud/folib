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
package com.folib.gremlin.adapters;

import com.folib.db.schema.Edges;
import com.folib.db.schema.Vertices;
import com.folib.domain.SecurityRole;
import com.folib.domain.User;
import com.folib.domain.UserEntity;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.EntityTraversalUtils;
import com.folib.gremlin.dsl.__;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static cn.hutool.core.convert.Convert.toLocalDateTime;
import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;
import static com.folib.gremlin.dsl.EntityTraversalUtils.toLong;
import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;

/**
 * @author veadan
 */
@Component
public class UserAdapter implements VertexEntityTraversalAdapter<User> {

    @Inject
    private SecurityRoleAdapter securityRoleAdapter;

    @Override
    public String label() {
        return Vertices.USER;
    }

    @Override
    public EntityTraversal<Vertex, User> fold() {
        return __.<Vertex, Object>project("id",
                "uuid",
                "password",
                "originalPassword",
                "enabled",
                "email",
                "userType",
                "roles",
                "securityTokenKey",
                "lastUpdated",
                "sourceId",
                "avatar")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("password"))
                .by(__.enrichPropertyValue("originalPassword"))
                .by(__.enrichPropertyValue("enabled"))
                .by(__.enrichPropertyValue("email"))
                .by(__.enrichPropertyValue("userType"))
                .by(__.outE(Edges.USER_HAS_SECURITY_ROLES)
                        .inV()
                        .map(securityRoleAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("securityTokenKey"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("sourceId"))
                .by(__.enrichPropertyValue("avatar"))
                .map(this::map);
    }

    private User map(Traverser<Map<String, Object>> t) {
        UserEntity result = new UserEntity(extractObject(String.class, t.get().get("uuid")));
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setPassword(extractObject(String.class, t.get().get("password")));
        result.setOriginalPassword(extractObject(String.class, t.get().get("originalPassword")));
        result.setEnabled(extractObject(Boolean.class, t.get().get("enabled")));
        result.setEmail(extractObject(String.class, t.get().get("email")));
        result.setUserType(extractObject(String.class, t.get().get("userType")));
        List<SecurityRole> userRoles = (List<SecurityRole>) t.get().get("roles");
        result.setRoles(new HashSet<>(userRoles));
        result.setSecurityTokenKey(extractObject(String.class, t.get().get("securityTokenKey")));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setSourceId(extractObject(String.class, t.get().get("sourceId")));
        result.setAvatar(extractObject(String.class, t.get().get("avatar")));
        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(User entity) {
        String storedUserId = Vertices.USER + ":" + UUID.randomUUID().toString();

        EntityTraversal<Vertex, Vertex> userRoleTraversal = __.identity();
        EntityTraversal<Vertex, Vertex> unfoldTraversal = __.identity();

        unfoldTraversal.sideEffect(__.outE(Edges.USER_HAS_SECURITY_ROLES).drop());

        for (SecurityRole securityRole : entity.getRoles()) {
            userRoleTraversal = userRoleTraversal.V(securityRole)
                    .saveV(securityRole.getUuid(),
                            securityRoleAdapter.unfold(securityRole));
            userRoleTraversal = userRoleTraversal.addE(Edges.USER_HAS_SECURITY_ROLES)
                    .from(__.<Vertex, Vertex>select(storedUserId).unfold())
                    .outV();
        }

        unfoldTraversal = unfoldTraversal.map(unfoldUser(entity))
                .store(storedUserId)
                .map(userRoleTraversal);

        return new UnfoldEntityTraversal<>(Vertices.USER, entity, unfoldTraversal);
    }

    private EntityTraversal<Vertex, Vertex> unfoldUser(User entity) {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (StringUtils.isNotBlank(entity.getPassword())) {
            t = t.property(single, "password", entity.getPassword());
        }
        if (StringUtils.isNotBlank(entity.getOriginalPassword())) {
            t = t.property(single, "originalPassword", entity.getOriginalPassword());
        }
        if (StringUtils.isNotBlank(entity.getUserType())) {
            t = t.property(single, "userType", entity.getUserType());
        }
        if (StringUtils.isNotBlank(entity.getEmail())) {
            t = t.property(single, "email", entity.getEmail());
        }
        if (StringUtils.isNotBlank(entity.getSecurityTokenKey())) {
            t = t.property(single, "securityTokenKey", entity.getSecurityTokenKey());
        }
        if (StringUtils.isNotBlank(entity.getSourceId())) {
            t = t.property(single, "sourceId", entity.getSourceId());
        }
        if (entity.getLastUpdated() != null) {
            t = t.property(single, "lastUpdated", toLong(entity.getLastUpdated()));
        }
        if (StringUtils.isNotBlank(entity.getAvatar())) {
            t = t.property(single, "avatar", entity.getAvatar());
        }
        t = t.property(single, "enabled", entity.isEnabled());

        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade() {
        return __.<Vertex>identity().map(t -> Element.class.cast(t.get()));
    }

}
