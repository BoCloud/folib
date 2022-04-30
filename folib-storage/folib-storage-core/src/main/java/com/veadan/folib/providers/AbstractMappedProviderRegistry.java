package com.veadan.folib.providers;

import java.util.LinkedHashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author carlspring
 */
public abstract class AbstractMappedProviderRegistry<T>
{
    
    private static final Logger logger = LoggerFactory.getLogger(AbstractMappedProviderRegistry.class);

    /**
     * K: The alias
     * V: The provider implementation
     */
    private Map<String, T> providers = new LinkedHashMap<>();


    public AbstractMappedProviderRegistry()
    {
    }

    public abstract void initialize();

    public Map<String, T> getProviders()
    {
        return providers;
    }

    public void setProviders(Map<String, T> providers)
    {
        providers.entrySet()
                 .stream()
                 .forEach(e -> logger.info("Registered repository provider '{}' with alias '{}'.",
                                           e.getValue().getClass().getCanonicalName(),
                                           e.getKey()));
        this.providers = providers;
    }

    public T getProvider(String alias)
    {
        return providers.get(alias);
    }

    public T addProvider(String alias, T provider)
    {
        return providers.put(alias, provider);
    }

    public void removeProvider(String alias)
    {
        providers.remove(alias);
    }

}
