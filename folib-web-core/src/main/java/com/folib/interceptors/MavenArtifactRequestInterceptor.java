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
package com.folib.interceptors;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.artifact.MavenArtifact;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.storage.repository.Repository;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.maven.index.context.IndexingContext;
import org.springframework.web.servlet.HandlerMapping;
import static com.folib.artifact.coordinates.MavenCoordinates.LAYOUT_NAME;
import static com.folib.storage.metadata.MetadataHelper.*;
import static org.springframework.http.HttpStatus.BAD_REQUEST;

/**
 * @author veadan
 */
public class MavenArtifactRequestInterceptor
        extends ArtifactRequestInterceptor
{

    private final RepositoryPathResolver repositoryPathResolver;

    public MavenArtifactRequestInterceptor(RepositoryPathResolver repositoryPathResolver)
    {
        super(LAYOUT_NAME);
        this.repositoryPathResolver = repositoryPathResolver;
    }

    @Override
    protected boolean preHandle(Repository repository,
                                String artifactPath,
                                HttpServletRequest request,
                                HttpServletResponse response)
            throws IOException
    {
        final Map pathVariables = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        if (pathVariables == null)
        {
            return true;
        }

        if (artifactPath.endsWith("/"))
        {
            response.sendError(BAD_REQUEST.value(), "The specified path should not ends with `/` character!");
            return false;
        }

        final boolean mavenArtifactRequest = isMavenArtifactRequest(request.getMethod());
        if (!mavenArtifactRequest)
        {
            return true;
        }
        final RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        if (Files.exists(repositoryPath) && Files.isDirectory(repositoryPath))
        {
            response.sendError(BAD_REQUEST.value(), "The specified path is a directory!");
            return false;
        }
        final String filename = repositoryPath.getFileName().toString();
        if (MAVEN_METADATA_XML.equals(filename) ||
            MAVEN_METADATA_XML_CHECKSUM_MD5.equals(filename) ||
            MAVEN_METADATA_XML_CHECKSUM_SHA1.equals(filename) ||
            MAVEN_METADATA_XML_CHECKSUM_SHA256.equals(filename) ||
            filename.startsWith(IndexingContext.INDEX_FILE_PREFIX))
        {
            return true;
        }
        boolean isValidGavPath = MavenArtifactUtils.isGAV(repositoryPath);
        if (!isValidGavPath)
        {
            response.sendError(BAD_REQUEST.value(), "The specified path is invalid. Maven GAV not recognized.");
            return false;
        }
        final MavenArtifact mavenArtifact = MavenArtifactUtils.convertPathToArtifact(repositoryPath);
        if (StringUtils.isBlank(mavenArtifact.getArtifactId()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact artifactId not recognized.");
            return false;
        }
        if (StringUtils.isBlank(mavenArtifact.getGroupId()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact groupId not recognized.");
            return false;
        }
        if (StringUtils.isBlank(mavenArtifact.getVersion()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact version not recognized.");
            return false;
        }
        if (StringUtils.isBlank(mavenArtifact.getType()))
        {
            response.sendError(BAD_REQUEST.value(),
                               "The specified path is invalid. Maven artifact type not recognized.");
            return false;
        }

        return true;
    }

    private static boolean isMavenArtifactRequest(final String method)
    {
        return "get".equalsIgnoreCase(method) ||
               "head".equalsIgnoreCase(method) ||
               "post".equalsIgnoreCase(method) ||
               "put".equalsIgnoreCase(method) ||
               "patch".equalsIgnoreCase(method);
    }

}
