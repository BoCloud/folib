package com.veadan.folib.artifact.coordinates;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.io.LayoutOutputStream;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream;
import com.veadan.folib.commons.io.RandomInputStream;

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
