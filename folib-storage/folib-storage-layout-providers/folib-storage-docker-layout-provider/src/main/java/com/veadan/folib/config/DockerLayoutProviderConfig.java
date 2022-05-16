package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.DockerFileSystem;
import com.veadan.folib.providers.layout.DockerFileSystemProvider;
import com.veadan.folib.providers.layout.DockerLayoutProvider;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;
import groovy.json.JsonOutput;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

@Configuration
@ComponentScan({"com.veadan.folib.configuration",
        "com.veadan.folib.repository",
        "com.veadan.folib.providers",
        "com.veadan.folib.services",
        "com.veadan.folib.storage"})
public class DockerLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + DockerLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            DockerLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;


    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory dockerRepositoryFileSystemProviderFactory() {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());
            return dockerFileSystemProvider(storageProvider.getFileSystemProvider());
        };

    }

    @Bean
    @Scope("prototype")
    public DockerFileSystemProvider dockerFileSystemProvider(FileSystemProvider provider) {
        return new DockerFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory dockerRepositoryFileSystemFactory(PropertiesBooter propertiesBooter) {
        LayoutFileSystemProviderFactory providerFactory = dockerRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return dockerRepositoryFileSystem(propertiesBooter,
                    repository,
                    storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public DockerFileSystem dockerRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                       Repository repository,
                                                       FileSystem storageFileSystem,
                                                       LayoutFileSystemProvider provider) {
        return new DockerFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

}
