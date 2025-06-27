package com.veadan.folib.providers.io;

import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.veadan.folib.util.ThrowingPredicate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author veadan
 * @date 2024/1/19
 **/
@Slf4j
@Component
public class DockerProxyRepositoryPathFetchEventListener {

    @Inject
    private List<DockerExpiredRepositoryPathHandler> expiredRepositoryPathHandlers;

    @AsyncEventListener
    public void handle(final ProxyRepositoryPathExpiredEvent event) {

        RepositoryPath repositoryPath = event.getPath();
        if (!DockerLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout())) {
            return;
        }

        expiredRepositoryPathHandlers.stream()
                .filter(ThrowingPredicate.unchecked((handler) -> handler.supports(repositoryPath)))
                .forEach(handleExpiration(repositoryPath));
    }

    private Consumer<DockerExpiredRepositoryPathHandler> handleExpiration(final RepositoryPath repositoryPath) {
        return handler ->
        {
            try {
                handler.handleExpiration(repositoryPath);
            } catch (IOException e) {
                log.error("Expired path [{}] improperly handled.", repositoryPath, e);
            }
        };
    }
}
