package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.layout.HelmFileSystem;
import com.veadan.folib.providers.layout.HelmFileSystemProvider;
import com.veadan.folib.providers.layout.HelmLayoutProvider;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
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
public class HelmLayoutProviderConfig {
    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + HelmLayoutProvider.ALIAS;
    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            HelmLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;


    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory helmRepositoryFileSystemProviderFactory() {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());
            return helmFileSystemProvider(storageProvider.getFileSystemProvider());
        };
    }

    @Bean
    @Scope("prototype")
    public HelmFileSystemProvider helmFileSystemProvider(FileSystemProvider provider) {
        return new HelmFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory helmRepositoryFileSystemFactory(PropertiesBooter propertiesBooter) {
        LayoutFileSystemProviderFactory providerFactory = helmRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return helmRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public HelmFileSystem helmRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                   Repository repository,
                                                   FileSystem storageFileSystem,
                                                   LayoutFileSystemProvider provider) {
        return new HelmFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }
}
