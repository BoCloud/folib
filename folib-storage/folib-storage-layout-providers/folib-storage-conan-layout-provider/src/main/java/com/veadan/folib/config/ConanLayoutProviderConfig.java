package com.veadan.folib.config;


import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.ConanFileSystem;
import com.veadan.folib.providers.layout.ConanFileSystemProvider;
import com.veadan.folib.providers.layout.ConanLayoutProvider;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

@Configuration
@ComponentScan({ "com.veadan.folib.configuration",
        "com.veadan.folib.repository",
        "com.veadan.folib.providers",
        "com.veadan.folib.services",
        "com.veadan.folib.storage" })
public class ConanLayoutProviderConfig {
    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + ConanLayoutProvider.ALIAS;
    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            ConanLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;


    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory helmRepositoryFileSystemProviderFactory() {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = conanFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };
    }

    @Bean
    @Scope("prototype")
    public ConanFileSystemProvider conanFileSystemProvider(FileSystemProvider provider) {
        return new ConanFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory conanRepositoryFileSystemFactory(PropertiesBooter propertiesBooter) {
        LayoutFileSystemProviderFactory providerFactory = helmRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return conanRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public ConanFileSystem conanRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                     Repository repository,
                                                     FileSystem storageFileSystem,
                                                     LayoutFileSystemProvider provider) {
        return new ConanFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }
}