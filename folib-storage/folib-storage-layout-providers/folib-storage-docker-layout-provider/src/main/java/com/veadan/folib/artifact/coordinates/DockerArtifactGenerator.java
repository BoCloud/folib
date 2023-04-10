package com.veadan.folib.artifact.coordinates;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.io.LayoutOutputStream;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.schema2.ContainerConfigurationManifest;
import com.veadan.folib.schema2.ImageManifest;
import com.veadan.folib.schema2.LayerManifest;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import org.carlspring.commons.io.RandomInputStream;

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
