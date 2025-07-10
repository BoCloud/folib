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
package com.folib.providers;

import com.folib.artifact.coordinates.RpmCoordinates;
import com.folib.domain.Artifact;
import com.folib.npm.metadata.PackageEntry;
import com.folib.npm.metadata.SearchResult;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Path;
import java.time.ZoneId;
import java.util.Date;
import java.util.function.Function;

@Component
public class RpmSearchResultSupplier implements Function<Path, SearchResult>
{

    private static final Logger logger = LoggerFactory.getLogger(RpmSearchResultSupplier.class);

    public static final String SEARCH_DATE_FORMAT = "EEE MMM dd yyyy HH:mm:SS ZZZ (zzz)";

    @Override
    public SearchResult apply(Path path)
    {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        RpmCoordinates c;
        Artifact artifactEntry;
        try
        {
            c = (RpmCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            artifactEntry = repositoryPath.getArtifactEntry();
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }

        SearchResult searchResult = new SearchResult();

        PackageEntry packageEntry = new PackageEntry();
        searchResult.setPackage(packageEntry);

        packageEntry.setDate(Date.from(artifactEntry.getLastUpdated().atZone(ZoneId.systemDefault()).toInstant()));

        packageEntry.setName(c.getName());
        packageEntry.setScope(c.getScope() == null ? "unscoped" : c.getScope());
        packageEntry.setVersion(c.getVersion());

        return searchResult;
    }

}
