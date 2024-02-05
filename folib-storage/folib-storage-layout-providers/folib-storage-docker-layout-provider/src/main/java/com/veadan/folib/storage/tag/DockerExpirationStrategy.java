package com.veadan.folib.storage.tag;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;

/**
 * @author leipenghui
 * @date 2024/1/19
 **/
public interface DockerExpirationStrategy {

    enum Decision {
        UNDECIDED, EXPIRED, USABLE
    }

    Decision decide(final RepositoryPath repositoryPath) throws IOException;

    default String fetchDigest(final RepositoryPath repositoryPath) throws IOException {
        return "";
    }
}
