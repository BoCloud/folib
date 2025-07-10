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
package com.folib.domain;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.folib.data.domain.DomainEntity;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Vertices;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Relationship;

/**
 * @author veadan
 * @author veadan
 */
@NodeEntity(Vertices.ARTIFACT_ID_GROUP)
public class ArtifactIdGroupEntity extends DomainEntity implements ArtifactIdGroup
{

    private String storageId;

    private String repositoryId;

    private String name;

    private String metadata;

    @Relationship(type = Edges.ARTIFACT_GROUP_HAS_ARTIFACTS, direction = Relationship.OUTGOING)
    private Set<Artifact> artifacts = new HashSet<>();


    ArtifactIdGroupEntity()
    {
    }

    public ArtifactIdGroupEntity(String storageId,
                                 String repositoryId,
                                 String name)
    {
        setUuid(String.format("%s-%s-%s", storageId, repositoryId, name));
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.name = name;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    @Override
    public Set<Artifact> getArtifacts()
    {
        return Collections.unmodifiableSet(artifacts);
    }

    @Override
    public void addArtifact(Artifact artifact)
    {
        artifacts.remove(artifact);
        artifacts.add(artifact);
    }

    @Override
    public void setArtifacts(Set<Artifact> artifacts) {
        this.artifacts = artifacts;
    }

    @Override
    public void removeArtifact(Artifact artifact)
    {
        artifacts.remove(artifact);
    }

    @Override
    public String getMetadata() {
        return metadata;
    }

    @Override
    public void setMetadata(String metadata) {
        this.metadata = metadata;
    }
}
