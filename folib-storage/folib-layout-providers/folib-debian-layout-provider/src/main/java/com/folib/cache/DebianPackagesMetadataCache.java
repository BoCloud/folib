package com.folib.cache;

import com.folib.domain.Artifact;
import com.folib.domain.DebianMetadata;
import com.folib.storage.repository.Repository;

/**
 * @author veadan
 * @since 2024-09-03 17:31
 */
public interface DebianPackagesMetadataCache {
    DebianMetadata get(Artifact artifact, Repository repo);

    void put(Artifact artifact, DebianMetadata metadata);

    void remove(String path);
}
