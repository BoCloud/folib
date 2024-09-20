package com.veadan.folib.cache;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.DebianMetadata;
import com.veadan.folib.storage.repository.Repository;

/**
 * @author huayanjun
 * @since 2024-09-03 17:31
 */
public interface DebianPackagesMetadataCache {
    DebianMetadata get(Artifact artifact, Repository repo);

    void put(Artifact artifact, DebianMetadata metadata);

    void remove(String path);
}
