package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.RawFileSystem;
import com.veadan.folib.providers.layout.RawFileSystemProvider;
import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;

import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
@ComponentScan({ "com.veadan.folib.repository",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage" })
public class RawLayoutProviderConfig
{

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + RawLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
                                                            RawLayoutProvider.ALIAS;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory rawRepositoryFileSystemProviderFactory(StorageProviderRegistry storageProviderRegistry)
    {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = rawFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public RawFileSystemProvider rawFileSystemProvider(FileSystemProvider provider)
    {
        return new RawFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory rawRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                  StorageProviderRegistry storageProviderRegistry)
    {
        LayoutFileSystemProviderFactory providerFactory = rawRepositoryFileSystemProviderFactory(storageProviderRegistry);
        
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return rawRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                                           providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public RawFileSystem rawRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                 Repository repository,
                                                 FileSystem storageFileSystem,
                                                 LayoutFileSystemProvider provider)
    {
        return new RawFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

}
