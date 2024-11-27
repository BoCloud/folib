package com.veadan.folib.providers.io;

import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.providers.repository.event.ProxyRepositoryPathExpiredEvent;
import com.veadan.folib.util.ThrowingPredicate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author pengYongQiang
 * @date 1/9/2024 16:56
 */
@Component
public class GoProxyRepositoryPathExpiredEventListener {

    @Inject
    private List<GoExpiredRepositoryPathHandler> goExpiredRepositoryPathHandlers;
    private static final Logger logger = LoggerFactory.getLogger(GoProxyRepositoryPathExpiredEventListener.class);

    @EventListener
    public void handle(final ProxyRepositoryPathExpiredEvent event)
    {

        RepositoryPath repositoryPath = event.getPath();
        if (!GoLayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout()))
        {
            return;
        }

        goExpiredRepositoryPathHandlers.stream()
                .filter(ThrowingPredicate.unchecked((handler) -> handler.supports(repositoryPath)))
                .forEach(handleExpiration(repositoryPath));
    }
    private Consumer<GoExpiredRepositoryPathHandler> handleExpiration(final RepositoryPath repositoryPath)
    {
        return handler ->
        {
            try
            {
                handler.handleExpiration(repositoryPath);
            }
            catch (IOException e)
            {
                logger.error("Expired path [{}] improperly handled.", repositoryPath, e);
            }
        };
    }
}
