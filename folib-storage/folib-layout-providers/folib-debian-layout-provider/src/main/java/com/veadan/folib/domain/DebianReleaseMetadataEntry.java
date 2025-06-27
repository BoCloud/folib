package com.veadan.folib.domain;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;

import java.io.Serializable;

/**
 * @author veadan
 * @since 2024-09-04 16:22
 */
@Data
@NoArgsConstructor
public class DebianReleaseMetadataEntry implements Serializable {
    public String sha1;
    public String sha256;
    public String md5sum;
    public String path;
    public long size;

    public DebianReleaseMetadataEntry(String relativePath, Artifact artifact, String md5sum) {
        this.path = relativePath;
        this.size = artifact.getSizeInBytes();
        this.sha1 = artifact.getChecksums().get(MessageDigestAlgorithms.SHA_1);
        this.sha256 = artifact.getChecksums().get(MessageDigestAlgorithms.SHA_256);
        this.md5sum = md5sum;
    }
}
