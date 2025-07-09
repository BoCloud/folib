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
package com.folib.storage.metadata.maven;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.commons.encryption.EncryptionAlgorithmsEnum;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class ChecksumMetadataExpirationStrategy
        implements MetadataExpirationStrategy
{

    private static final Logger logger = LoggerFactory.getLogger(ChecksumMetadataExpirationStrategy.class);

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    public Decision decide(final RepositoryPath repositoryPath) throws IOException
    {
        Decision decision = decideUsingChecksumAlgorithm(repositoryPath, EncryptionAlgorithmsEnum.SHA1);
        if (Decision.UNDECIDED.equals(decision))
        {
            decision = decideUsingChecksumAlgorithm(repositoryPath, EncryptionAlgorithmsEnum.MD5);
        }
        return decision;
    }

    private RepositoryPath resolveSiblingChecksum(final RepositoryPath repositoryPath,
                                                  final EncryptionAlgorithmsEnum checksumAlgorithm)
    {
        return repositoryPath.resolveSibling(
                repositoryPath.getFileName().toString() + checksumAlgorithm.getExtension());
    }

    private String readChecksum(final RepositoryPath checksumRepositoryPath)
            throws IOException
    {
        if (!Files.exists(checksumRepositoryPath))
        {
            return null;
        }

        return Files.readAllLines(checksumRepositoryPath).stream().findFirst().orElse(null);
    }

    private Decision decideUsingChecksumAlgorithm(final RepositoryPath repositoryPath,
                                                  final EncryptionAlgorithmsEnum checksumAlgorithm) throws IOException
    {
        RepositoryPath checksumRepositoryPath = resolveSiblingChecksum(repositoryPath, checksumAlgorithm);
        String currentChecksum = readChecksum(checksumRepositoryPath);
        if (currentChecksum == null)
        {
            logger.info("Unable to read local {} checksum for {}, returning " + Decision.UNDECIDED.name(),
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.UNDECIDED;
        }

        proxyRepositoryArtifactResolver.fetchRemoteResource(checksumRepositoryPath);
        final String newRemoteChecksum = readChecksum(checksumRepositoryPath);

        if (newRemoteChecksum == null)
        {
            logger.info("Unable to fetch remote {} checksum for {}, returning " + Decision.UNDECIDED.name(),
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.UNDECIDED;
        }

        if (currentChecksum.equals(newRemoteChecksum))
        {
            logger.info("Local and remote {} checksums match for {}, no need to refetch metadata",
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.USABLE;
        }
        else
        {
            logger.info("Local and remote {} checksums differ for {}, will refetch metadata",
                         checksumAlgorithm,
                         repositoryPath.normalize());
            return Decision.EXPIRED;
        }
    }

}
