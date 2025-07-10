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
package com.folib.artifact.coordinates;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import com.folib.commons.io.RandomInputStream;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.security.NoSuchAlgorithmException;

public class RpmArtifactGenerator {
    private Path basePath;

    private Path imageManifestPath;

    private String imageManifestDigest;

    private Path configPath;

    private ObjectMapper mapper = new ObjectMapper();

    public RpmArtifactGenerator(String basedir)
    {
        this.basePath = Paths.get(basedir);
    }

    public void buildArtifact(String id,
                              String version)
            throws IOException,
            NoSuchAlgorithmException
    {

    }

    private String getDigest(String sha256)
    {
        return "sha256:" + sha256;
    }

    private void writeLayer(TarArchiveOutputStream tarOut)
            throws IOException,
            UnsupportedEncodingException
    {
        Path tempLayerPath = getLayerPath("layer.tmp");
        Files.createDirectories(tempLayerPath.getParent());

        try {
            try (OutputStream out = new BufferedOutputStream(Files.newOutputStream(tempLayerPath, StandardOpenOption.CREATE)))
            {
                RandomInputStream ris = new RandomInputStream(true, 1000000);
                byte[] buffer = new byte[4096];
                int len;
                while ((len = ris.read(buffer)) > 0)
                {
                    out.write(buffer, 0, len);
                }
                ris.close();
            }

            TarArchiveEntry entry = new TarArchiveEntry(tempLayerPath.toFile(), "layer");
            tarOut.putArchiveEntry(entry);

            Files.copy(tempLayerPath, tarOut);

            tarOut.closeArchiveEntry();
        } finally {
            Files.delete(tempLayerPath);
        }
    }

    /**
     * @param layer
     *            - identifies a layer by either a digest or some string
     * @return path to the generated layer file
     */
    public Path getLayerPath(String layer)
    {
        return basePath.resolve("layers/" + layer);
    }

    public Path getImageManifestPath()
    {
        return imageManifestPath;
    }

    public String getImageManifestDigest()
    {
        return imageManifestDigest;
    }

    public Path getConfigPath()
    {
        return configPath;
    }

    private String toUtf8(byte[] digest)
    {
        return new String(digest, StandardCharsets.UTF_8);
    }

}
