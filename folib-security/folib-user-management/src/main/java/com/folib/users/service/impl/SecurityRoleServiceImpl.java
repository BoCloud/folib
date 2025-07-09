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
package com.folib.users.service.impl;

import com.folib.data.CacheName;
import com.folib.gremlin.dsl.EntityTraversalSource;
import com.folib.repositories.SecurityRoleRepository;
import com.folib.users.service.SecurityRoleService;
import com.folib.domain.SecurityRole;
import com.folib.domain.SecurityRoleEntity;

import javax.inject.Inject;

import java.lang.reflect.UndeclaredThrowableException;
import java.util.Optional;

import org.apache.tinkerpop.gremlin.structure.Graph;
import org.janusgraph.core.JanusGraph;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author veadan
 */
@Service
@Transactional
public class SecurityRoleServiceImpl implements SecurityRoleService
{

    @Inject
    private JanusGraph janusGraph;

    @Inject
    private SecurityRoleRepository securityRoleRepository;

    @Override
    @Cacheable(value = CacheName.User.SECURITY_ROLES, key = "#roleName", sync = true)
    public SecurityRole findOneOrCreate(String roleName)
    {
        Optional<SecurityRole> securityRole = securityRoleRepository.findById(roleName);

        return securityRole.orElseGet(() -> {

            Graph g = janusGraph.tx().createThreadedTx();
            try
            {
                SecurityRoleEntity userRoleEntity = securityRoleRepository.save(() -> g.traversal(EntityTraversalSource.class),
                                                                                new SecurityRoleEntity(roleName));
                g.tx().commit();

                return userRoleEntity;
            }
            catch (Exception e)
            {
                g.tx().rollback();
                throw new UndeclaredThrowableException(e);
            } 
            finally
            {
                g.tx().close();
            }

        });
    }
}
