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

import com.folib.artifact.coordinates.PhpCoordinates;
import com.folib.domain.Artifact;
import com.folib.php.PhpSearchPackage;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Path;
import java.util.function.Function;

/**
 * @author veadan
 */
@Component
public class PhpSearchResultSupplier implements Function<Path, PhpSearchPackage> {

    private static final Logger logger = LoggerFactory.getLogger(PhpSearchResultSupplier.class);

    public static final String SEARCH_DATE_FORMAT = "EEE MMM dd yyyy HH:mm:SS ZZZ (zzz)";

    @Override
    public PhpSearchPackage apply(Path path) {
        RepositoryPath repositoryPath = (RepositoryPath) path;

        PhpCoordinates c;
        Artifact artifactEntry;
        try {
            c = (PhpCoordinates) RepositoryFiles.readCoordinates(repositoryPath);
            artifactEntry = repositoryPath.getArtifactEntry();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }
        PhpSearchPackage phpSearchPackage = PhpSearchPackage.builder().name(c.getName()).description(c.getDescription()).build();
        return phpSearchPackage;
    }

}
