package com.veadan.folib.providers;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author carlspring
 */
public abstract class AbstractMappedProviderRegistryWithNestedMap<T>
{

    /**
     * K: The alias
     * V: The provider implementation map
     */
    protected Map<String, Map<String, T>> providers;


    public AbstractMappedProviderRegistryWithNestedMap()
    {
    }

    public abstract void initialize();

    public Map<String, Map<String, T>> getProviders()
    {
        return providers;
    }

    public void setProviders(Map<String, Map<String, T>> providers)
    {
        this.providers = providers;
    }

    public Map<String, T> getProviderImplementations(String alias)
    {
        return providers.get(alias);
    }

    public T getProviderImplementation(String alias, String implementation)
            throws ProviderImplementationException
    {
        Map<String, T> map = providers.get(alias);
        if (map != null)
        {
            return map.get(implementation);
        }
        else
        {
            throw new ProviderImplementationException("The requested implementation '" + implementation + "'" +
                                                      " was not found for provider '" + alias  + "'.");
        }
    }

    public void addProviderImplementation(String alias, String implementation, T provider)
    {
        Map<String, T> map = providers.containsKey(alias) ? providers.get(alias) : new LinkedHashMap<>();
        map.put(implementation, provider);

        providers.put(alias, map);
    }

    public void removeProviderImplementation(String alias, String implementation)
    {
        providers.get(alias).remove(implementation);
    }

}
