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
package com.folib.indexer;

import com.folib.constant.DebianConstant;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.domain.Artifact;
import com.folib.gremlin.adapters.ArtifactAdapter;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import com.folib.storage.repository.Repository;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author veadan
 * @since 2024-09-07 15:34
 */
@Component
public class ArtifactorySearch extends GremlinVertexRepository<Artifact> {


    @Resource
    ArtifactAdapter artifactAdapter;

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }


    public List<Artifact> findReleaseByDistribution(String distribution, Repository repo) {
        List<Artifact> artifacts = g().V().hasLabel(Vertices.ARTIFACT)
                .has(Properties.REPOSITORY_ID, repo.getId())
                .has(Properties.STORAGE_ID, repo.getStorage().getId())
                .map(artifactAdapter.fold()).toList();
        return artifacts.stream()
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get("extension").equals(DebianConstant.PACKAGE_EXTENSION))
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.DISTRIBUTION).equals(distribution))
                .collect(Collectors.toList());

    }

    public List<Artifact> findByDistributionAndComponent(String distribution, String component, Repository repo) {
        List<Artifact> artifacts = g().V().hasLabel(Vertices.ARTIFACT)
                .has(Properties.REPOSITORY_ID, repo.getId())
                .has(Properties.STORAGE_ID, repo.getStorage().getId())
                .map(artifactAdapter.fold()).toList();
        return artifacts.stream()
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.DISTRIBUTION).equals(distribution))
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.COMPONENT).equals(component))
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.NAME).equals("Packages"))
                .collect(Collectors.toList());
    }

    public List<Artifact> findAllPackage(Repository repo) {
        List<Artifact> artifacts = g().V().hasLabel(Vertices.ARTIFACT)
                .has(Properties.REPOSITORY_ID, repo.getId())
                .has(Properties.STORAGE_ID, repo.getStorage().getId())
                .map(artifactAdapter.fold()).toList();
        return artifacts.stream()
                .filter(e -> e.getArtifactCoordinates().getCoordinates().get(DebianConstant.NAME).equals("Packages"))
                .collect(Collectors.toList());
    }

}
