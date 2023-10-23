package com.veadan.folib.providers.io;

import com.veadan.folib.providers.repository.RepositoryProvider;
import lombok.extern.slf4j.Slf4j;

import java.nio.file.Path;
import java.util.concurrent.Callable;

/**
 * @author leipenghui
 * @date 2023/10/23
 **/
@Slf4j
public class MavenGroupRepositoryPathFetchTask implements Callable<Path> {

    private RepositoryProvider provider;

    private RepositoryPath resolvedPath;

    public MavenGroupRepositoryPathFetchTask(RepositoryProvider provider, RepositoryPath resolvedPath) {
        this.provider = provider;
        this.resolvedPath = resolvedPath;
    }

    @Override
    public Path call() throws Exception {
        return provider.fetchPath(resolvedPath);
    }
}
