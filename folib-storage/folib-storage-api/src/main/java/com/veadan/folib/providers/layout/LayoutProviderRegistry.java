package com.veadan.folib.providers.layout;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class LayoutProviderRegistry
        extends AbstractMappedProviderRegistry<LayoutProvider>
{

    private static final Logger logger = LoggerFactory.getLogger(LayoutProviderRegistry.class);

    @Lazy
    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    @Lazy
    private Optional<List<LayoutProvider>> layoutProviders;

    public static LayoutProvider getLayoutProvider(Repository repository,
                                                   LayoutProviderRegistry layoutProviderRegistry)
            throws ProviderImplementationException
    {
        return layoutProviderRegistry.getProvider(repository.getLayout());
    }

    @Override
    @PostConstruct
    public void initialize()
    {
        layoutProviders.ifPresent(providers -> providers.stream().forEach(lp -> addProvider(lp.getAlias(), lp)));
        logger.info("Initialized the layout provider registry.");
    }

    public Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

    public Storage getStorage(String storageId)
    {
        return configurationManager.getConfiguration().getStorage(storageId);
    }

}
