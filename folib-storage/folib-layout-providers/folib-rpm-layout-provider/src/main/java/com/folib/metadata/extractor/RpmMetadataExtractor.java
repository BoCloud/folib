package com.folib.metadata.extractor;


import org.redline_rpm.header.Format;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class RpmMetadataExtractor  {
    public RpmMetadata extract(Path repositoryPath) throws Exception {
        if (repositoryPath == null) {
            return null;
        } else {
            Format rpmFormat = RpmFormatReader.read(repositoryPath.toString());
            if (rpmFormat == null) {
                return null;
            } else {
                RpmMetadata metadata = new RpmFormatInterpreter().interpret(rpmFormat);
                Path shaPath = Path.of(repositoryPath.toString());
                metadata.setSha1Digest(readSHA1FileContent(shaPath));
                metadata.setArtifactRelativePath(repositoryPath.getFileName().toString());
                metadata.setLastModified(getlastModified(repositoryPath));
                metadata.setSize(getFileSize(repositoryPath));
                return metadata;
            }
        }
    }

    public  String readSHA1FileContent(Path filePath) throws Exception {
        try {
            //return getSHA1(filePath);
            return Files.readString(Paths.get(filePath.toString()+".sha1"));
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

    public  String getSHA1(Path filePath) throws NoSuchAlgorithmException, IOException {
        MessageDigest digest = MessageDigest.getInstance("SHA-1");
        try (var in = Files.newInputStream(filePath)) {
            byte[] buffer = new byte[4096];
            int read;
            while ((read = in.read(buffer)) > 0) {
                digest.update(buffer, 0, read);
            }
        }
        byte[] hash = digest.digest();
        return bytesToHex(hash);
    }

    private  String bytesToHex(byte[] hash) {
        StringBuilder hexString = new StringBuilder(2 * hash.length);
        for (int i = 0; i < hash.length; i++) {
            String hex = Integer.toHexString(0xff & hash[i]);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }
}