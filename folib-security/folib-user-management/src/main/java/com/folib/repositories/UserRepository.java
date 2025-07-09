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

import com.folib.db.schema.Edges;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.domain.SecurityRole;
import com.folib.domain.User;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.adapters.UserAdapter;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import jakarta.transaction.Transactional;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;
import java.util.List;
import java.util.stream.Collectors;

@Repository
@Transactional
public class UserRepository extends GremlinVertexRepository<User> {

    @Inject
    UserAdapter adapter;

    @Override
    protected EntityTraversalAdapter<Vertex, User> adapter() {
        return adapter;
    }

    public List<User> findUsersWithRole(String role) {
        return g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, role).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                .has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0)).has(Properties.ENABLED, true).map(adapter.fold()).dedup().toList();
    }

    public List<User> findUsersWithRoles(List<String> roleList) {
        return g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, P.within(roleList)).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                .has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0)).has(Properties.ENABLED, true).map(adapter.fold()).dedup().toList();
    }

    @Override
    public Iterable<User> findAll() {
        return g().V().hasLabel(Vertices.USER).has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0)).map(adapter.fold()).toList();
    }

    public List<User> findUsersPage(User user, int start, int end) {
        return commonUserPage(user).range(start, end).map(adapter.fold()).dedup().toList();
    }

    public Long countUsers(User user) {
        return commonUserPage(user).count().tryNext().orElse(0L);
    }

    private EntityTraversal<Vertex, Vertex> commonUserPage(User user) {
        EntityTraversal<Vertex, Vertex> entityTraversal;
        if (CollectionUtils.isNotEmpty(user.getRoles())) {
            entityTraversal = g().V().hasLabel(Vertices.SECURITY_ROLE).has(Properties.UUID, P.within(user.getRoles().stream().map(SecurityRole::getRoleName).collect(Collectors.toList()))).inE(Edges.USER_HAS_SECURITY_ROLES).outV()
                    .has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0));
        } else {
            entityTraversal = g().V().hasLabel(Vertices.USER).has(Properties.USER_TYPE, "general").has(Properties.CREATED, P.gt(0));
        }
        if (StringUtils.isNotBlank(user.getUsername())) {
            entityTraversal = entityTraversal.has(Properties.UUID, Text.textContains(user.getUsername()));
        }
        if (StringUtils.isNotBlank(user.getEmail())) {
            entityTraversal = entityTraversal.has(Properties.EMAIL, Text.textContains(user.getEmail()));
        }
        return entityTraversal;
    }

}
