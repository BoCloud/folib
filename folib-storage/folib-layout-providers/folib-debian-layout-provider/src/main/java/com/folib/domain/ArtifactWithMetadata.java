package com.folib.domain;

/**
 * @author veadan
 * @since 2024-09-03 17:25
 */
public class ArtifactWithMetadata {
    private final Artifact artifact;
    private final DebianMetadata metadata;

    public ArtifactWithMetadata(Artifact artifact, DebianMetadata metadata) {
        this.artifact = artifact;
        this.metadata = metadata;
    }

    public Artifact getArtifact() {
        return this.artifact;
    }

    public DebianMetadata getMetadata() {
        return this.metadata;
    }
}
