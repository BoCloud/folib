package com.folib.providers.io;

import com.folib.event.AsyncEventListener;
import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.folib.util.ThrowingPredicate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author veadan
 */
@Component
public class MavenProxyRepositoryPathExpiredEventListener {

    private static final Logger logger = LoggerFactory.getLogger(MavenProxyRepositoryPathExpiredEventListener.class);

    @Inject
    private List<MavenExpiredRepositoryPathHandler> expiredRepositoryPathHandlers;

    @AsyncEventListener
    public void handle(final ProxyRepositoryPathExpiredEvent event) {

        RepositoryPath repositoryPath = event.getPath();
        if (!Maven2LayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout())) {
            return;
        }

        expiredRepositoryPathHandlers.stream()
                .filter(ThrowingPredicate.unchecked((handler) -> handler.supports(repositoryPath)))
                .forEach(handleExpiration(repositoryPath));
    }

    private Consumer<MavenExpiredRepositoryPathHandler> handleExpiration(final RepositoryPath repositoryPath) {
        return handler ->
        {
            try {
                handler.handleExpiration(repositoryPath);
            } catch (IOException e) {
                logger.error("Expired path [{}] improperly handled.", repositoryPath, e);
            }
        };
    }
}
