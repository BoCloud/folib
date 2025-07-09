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
import com.folib.commons.io.RandomInputStream;
import com.folib.io.LayoutOutputStream;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.schema2.ContainerConfigurationManifest;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;

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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class DockerArtifactGenerator
{
    private Path basePath;

    private Path imageManifestPath;
    
    private String imageManifestDigest;

    private Path configPath;

    private ObjectMapper mapper = new ObjectMapper();

    public DockerArtifactGenerator(String basedir)
    {
        this.basePath = Paths.get(basedir);
    }

    public void buildArtifact(String id,
                              String version)
        throws IOException,
        NoSuchAlgorithmException
    {
        Files.createDirectories(basePath);

        imageManifestPath = basePath.resolve("distribution.manifest.v2.json");

        configPath = basePath.resolve("container.image.v1.json");

        ContainerConfigurationManifest config = new ContainerConfigurationManifest();

        List<LayerManifest> layers = new ArrayList<LayerManifest>();

        try (LayoutOutputStream configOutput = new LayoutOutputStream(
                new BufferedOutputStream(Files.newOutputStream(configPath, StandardOpenOption.CREATE))))
        {
            configOutput.addAlgorithm(MessageDigestAlgorithms.SHA_256);
            configOutput.setDigestStringifier(this::toUtf8);

            configOutput.write(mapper.writeValueAsBytes(new HashMap<String, String>()));

            String sha256 = configOutput.getDigestMap(DockerLayoutProvider.ALIAS).get(MessageDigestAlgorithms.SHA_256);
            String digest = getDigest(sha256);
            config.setDigest(digest);
        }

        Path tempLayerArchivePath = getLayerPath("layer.tar.gz.tmp");
        Files.createDirectories(tempLayerArchivePath.getParent());
        
        String layerDigest;
        
        try {
            try (
                    LayoutOutputStream layerOutput = new LayoutOutputStream(
                            new BufferedOutputStream(
                                    Files.newOutputStream(tempLayerArchivePath, StandardOpenOption.CREATE)));
                    GzipCompressorOutputStream gzipOut = new GzipCompressorOutputStream(layerOutput);
                    TarArchiveOutputStream tarOut = new TarArchiveOutputStream(gzipOut))
            {
                layerOutput.addAlgorithm(MessageDigestAlgorithms.SHA_256);
                layerOutput.setDigestStringifier(this::toUtf8);
                
                writeLayer(tarOut);
                
                String sha256 = layerOutput.getDigestMap(DockerLayoutProvider.ALIAS).get(MessageDigestAlgorithms.SHA_256);
                layerDigest = getDigest(sha256);
                
                LayerManifest layer = new LayerManifest();
                layer.setDigest(layerDigest);
                
                layers.add(layer);
            }
            
            Files.move(tempLayerArchivePath, getLayerPath(layerDigest));
        } finally {
            Files.deleteIfExists(tempLayerArchivePath);
        }
        
        ImageManifest imageManifest = new ImageManifest();
        imageManifest.setConfig(config);
        imageManifest.setLayers(layers);
        
        try (LayoutOutputStream manifestOutput = new LayoutOutputStream(
                new BufferedOutputStream(Files.newOutputStream(imageManifestPath, StandardOpenOption.CREATE))))
        {
            manifestOutput.addAlgorithm(MessageDigestAlgorithms.SHA_256);
            manifestOutput.setDigestStringifier(this::toUtf8);

            manifestOutput.write(mapper.writeValueAsBytes(imageManifest));

            String sha256 = manifestOutput.getDigestMap(DockerLayoutProvider.ALIAS).get(MessageDigestAlgorithms.SHA_256);
            imageManifestDigest = getDigest(sha256); 
        }
        // TODO: set media types and schema versions to v2 / schema 2
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
