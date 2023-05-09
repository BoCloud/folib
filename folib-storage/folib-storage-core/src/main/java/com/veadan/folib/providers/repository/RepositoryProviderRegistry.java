package com.veadan.folib.providers.repository;

import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.veadan.folib.providers.AbstractMappedProviderRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component("repositoryProviderRegistry")
public class RepositoryProviderRegistry extends AbstractMappedProviderRegistry<RepositoryProvider>
{

    private static final Logger logger = LoggerFactory.getLogger(RepositoryProviderRegistry.class);


    public RepositoryProviderRegistry()
    {
    }

    @Override
    @PostConstruct
    public void initialize()
    {
        logger.info("Initialized the repository provider registry.");
    }

    public void dump()
    {
        logger.info("Listing repository providers:");
        for (String providerName : getProviders().keySet())
        {
            logger.info(" provider: {}", providerName);
        }
    }

    /**
     * K: String   :
     * V: Provider : The provider
     *
     * @return
     */
    @Override
    public Map<String, RepositoryProvider> getProviders()
    {
        return super.getProviders();
    }

    @Inject
    @Override
    public void setProviders(Map<String, RepositoryProvider> providers)
    {
        super.setProviders(providers.entrySet()
                                    .stream()
                                    .collect(Collectors.toMap(e -> e.getKey()
                                                                    .replace(RepositoryProvider.class.getSimpleName(),
                                                                             ""),
                                                              e -> e.getValue())));
    }

    @Override
    public RepositoryProvider getProvider(String alias)
    {
        return super.getProvider(alias);
    }

    @Override
    public RepositoryProvider addProvider(String alias, RepositoryProvider provider)
    {
        return super.addProvider(alias, provider);
    }

    @Override
    public void removeProvider(String alias)
    {
        super.removeProvider(alias);
    }

}
