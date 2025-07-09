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
package com.folib.components;

/**
 * @author veadan
 * @date 2024/7/2
 **/

import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.pypi.PypiSearchResult;
import com.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

@Component
public class PypiBrowsePackageHtmlResponseBuilder {

    public String getHtmlResponse(List<Path> filePaths)
            throws IOException {

        String htmlResponse = "";
        if (CollectionUtils.isEmpty(filePaths)) {
            htmlResponse = "<!DOCTYPE html>\n<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Not Found</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Not Found</h1>\n" +
                    "        </body>\n" +
                    "</html>";
        } else {

            PypiCoordinates artifactCoordinates = (PypiCoordinates) RepositoryFiles.readCoordinates((RepositoryPath) filePaths.get(0));
            final String packageName = artifactCoordinates.getId();

            htmlResponse = "<!DOCTYPE html>\n<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Links for " + packageName + "</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Links for " + packageName + "</h1>\n" +
                    "                   " + getPackageLinks(filePaths) +
                    "        </body>\n" +
                    "</html>";
        }
        return htmlResponse;
    }

    private String getPackageLinks(List<Path> filePaths)
            throws IOException {

        StringBuilder packageLinks = new StringBuilder();

        for (Path path : filePaths) {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            PypiCoordinates artifactCoordinates = (PypiCoordinates) RepositoryFiles.readCoordinates(repositoryPath);

            Repository repository = repositoryPath.getRepository();
            packageLinks.append("<a href=\"" + "/storages/").append(repository.getStorage().getId()).append("/").append(repository.getId()).append("/packages/").append(artifactCoordinates.buildPath()).append("\">").append(artifactCoordinates.getFileName()).append("</a><br>\n");
        }

        return packageLinks.toString();
    }

    public String getProxyHtmlResponse(List<PypiSearchResult> pypiSearchResultList) {
        String htmlResponse = "";
        if (CollectionUtils.isEmpty(pypiSearchResultList)) {
            htmlResponse = "<!DOCTYPE html>\n<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Not Found</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Not Found</h1>\n" +
                    "        </body>\n" +
                    "</html>";
        } else {
            PypiCoordinates artifactCoordinates = PypiCoordinates.parse(pypiSearchResultList.get(0).getArtifactName());
            final String packageName = artifactCoordinates.getId();
            htmlResponse = "<!DOCTYPE html>\n<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Links for " + packageName + "</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Links for " + packageName + "</h1>\n" +
                    "                   " + getProxyPackageLinks(pypiSearchResultList) +
                    "        </body>\n" +
                    "</html>";
        }
        return htmlResponse;
    }

    private String getProxyPackageLinks(List<PypiSearchResult> pypiSearchResultList) {
        StringBuilder packageLinks = new StringBuilder();
        PypiCoordinates artifactCoordinates = null;
        for (PypiSearchResult pypiSearchResult : pypiSearchResultList) {
            artifactCoordinates = PypiCoordinates.parse(pypiSearchResult.getArtifactName());
            packageLinks.append("<a href=\"" + "/storages/").append(pypiSearchResult.getStorageId()).append("/").append(pypiSearchResult.getRepositoryId()).append("/packages/").append(pypiSearchResult.getArtifactPath()).append("\"");
            if (StringUtils.isNotBlank(pypiSearchResult.getAttributes())) {
                packageLinks.append(" ");
                packageLinks.append(pypiSearchResult.getAttributes());
            }
            packageLinks.append(">");
            packageLinks.append(artifactCoordinates.getFileName()).append("</a><br>\n");
        }
        return packageLinks.toString();
    }

    public String nouFound() {
        return "<!DOCTYPE html>\n<html>\n" +
                "        <head>\n" +
                "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                "            <title>Not Found</title>\n" +
                "        </head>\n" +
                "        <body>\n" +
                "            <h1>Not Found</h1>\n" +
                "        </body>\n" +
                "</html>";
    }

}
