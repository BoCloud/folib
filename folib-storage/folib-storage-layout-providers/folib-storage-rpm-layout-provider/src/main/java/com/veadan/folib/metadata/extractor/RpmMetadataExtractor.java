package com.veadan.folib.metadata.extractor;


import com.veadan.folib.providers.io.RepositoryPath;
import org.redline_rpm.header.Format;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

public class RpmMetadataExtractor  {
    public RpmMetadata extract(Path repositoryPath) throws IOException {
        if (repositoryPath == null) {
            return null;
        } else {
            Format rpmFormat = RpmFormatReader.read(repositoryPath.toString());
            if (rpmFormat == null) {
                return null;
            } else {
                RpmMetadata metadata = new RpmFormatInterpreter().interpret(rpmFormat);
                Path shaPath = Path.of(repositoryPath.toString() + ".sha1");
                metadata.setSha1Digest(readSHA1FileContent(shaPath));
                metadata.setArtifactRelativePath(repositoryPath.getFileName().toString());
                metadata.setLastModified(getlastModified(repositoryPath));
                metadata.setSize(getFileSize(repositoryPath));
                return metadata;
            }
        }
    }

    public  String readSHA1FileContent(Path filePath) throws IOException {
        try {
            return new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw e;
        }
    }

    public long getFileSize(Path filePath) throws IOException {
        return Files.size(filePath);
    }

    public long getlastModified(Path filePath) throws IOException {
        return Files.getLastModifiedTime(filePath).toMillis();
    }
}