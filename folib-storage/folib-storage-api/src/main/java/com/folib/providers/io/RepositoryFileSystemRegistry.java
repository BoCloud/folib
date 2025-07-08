package com.folib.providers.io;

import com.folib.storage.repository.Repository;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RepositoryFileSystemRegistry
{

    private Map<String, LayoutFileSystemProviderFactory> fileSystemProviderFactoryMap = new HashMap<>();

    private Map<String, LayoutFileSystemFactory> fileSystemFactoryMap = new HashMap<>();

    @Autowired(required = false)
    public void setFileSystemProviderFactories(Map<String, LayoutFileSystemProviderFactory> factories)
    {
        factories.entrySet()
                 .stream()
                 .forEach(e -> fileSystemProviderFactoryMap.put(extractLayoutAlias(LayoutFileSystemProviderFactory.class,
                                                                                   e.getKey()),
                                                                e.getValue()));
    }

    @Autowired(required = false)
    public void setFileSystemFactories(Map<String, LayoutFileSystemFactory> factories)
    {
        factories.entrySet()
                 .stream()
                 .forEach(e -> fileSystemFactoryMap.put(extractLayoutAlias(LayoutFileSystemFactory.class,
                                                                           e.getKey()),
                                                        e.getValue()));
    }

    private String extractLayoutAlias(Class<?> factoryClass,
                                      String alias)
    {
        return alias.replace(factoryClass.getSimpleName(), "").substring(1);
    }

    public LayoutFileSystemFactory lookupRepositoryFileSystemFactory(Repository r)
    {
        return fileSystemFactoryMap.get(r.getLayout());
    }

    public LayoutFileSystemProviderFactory lookupRepositoryFileSystemProviderFactory(Repository r)
    {
        return fileSystemProviderFactoryMap.get(r.getLayout());
    }
}
