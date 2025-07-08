package com.folib.config;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.layout.HelmFileSystem;
import com.folib.providers.layout.HelmFileSystemProvider;
import com.folib.providers.layout.HelmLayoutProvider;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;


@Configuration
@ComponentScan({ "com.folib.configuration",
        "com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage" })
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
