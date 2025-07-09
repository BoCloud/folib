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
package com.folib.artifact.archive;

import com.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * @author veadan
 */
public class CompositeArchiveListingFunction
        implements ArchiveListingFunction {

    private final Set<ArchiveListingFunction> leafs;

    public CompositeArchiveListingFunction(final Set<ArchiveListingFunction> leafs) {
        Objects.requireNonNull(leafs, "Set of archive listing functions should not be null");
        this.leafs = leafs;
    }

    @Override
    public Set<String> listFilenames(final RepositoryPath path)
            throws IOException {
        final Set<String> result = new HashSet<>();
        for (final ArchiveListingFunction leaf : leafs) {
            if (leaf.supports(path)) {
                result.addAll(leaf.listFilenames(path));
            }
        }
        return result;
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath path, String fileName) throws IOException {
        String content = "";
        for (final ArchiveListingFunction leaf : leafs) {
            if (leaf.supports(path)) {
                return leaf.getContentByFileName(path, fileName);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        for (final ArchiveListingFunction leaf : leafs) {
            if (leaf.supports(repositoryPath)) {
                return leaf.getContentByFileName(repositoryPath, path, fileName);
            }
        }
        return null;
    }

    @Override
    public byte[] getContentByEqualsFileName(RepositoryPath repositoryPath, Path path, String fileName) throws IOException {
        for (final ArchiveListingFunction leaf : leafs) {
            if (leaf.supports(repositoryPath)) {
                return leaf.getContentByEqualsFileName(repositoryPath, path, fileName);
            }
        }
        return null;
    }

    @Override
    public boolean supports(final RepositoryPath path) {
        return leafs.stream().filter(leaf -> leaf.supports(path)).findFirst().isPresent();
    }

    @Override
    public String toString() {
        return "[" + getClass().getName() + "] leafs {" + Objects.toString(leafs) + "}";
    }
}
