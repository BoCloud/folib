package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.Maven2FileSystemProvider;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.providers.layout.MavenFileSystem;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
@ComponentScan({ "com.veadan.folib.configuration",
                 "com.veadan.folib.repository",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage" })
public class Maven2LayoutProviderConfig
{

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + Maven2LayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
                                                            Maven2LayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;


    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory mavenRepositoryFileSystemProviderFactory()
    {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());
            return mavenFileSystemProvider(storageProvider.getFileSystemProvider());
        };

    }

    @Bean
    @Scope("prototype")
    public Maven2FileSystemProvider mavenFileSystemProvider(FileSystemProvider provider)
    {
        return new Maven2FileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory mavenRepositoryFileSystemFactory(PropertiesBooter propertiesBooter)
    {
        LayoutFileSystemProviderFactory providerFactory = mavenRepositoryFileSystemProviderFactory();
        
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return mavenRepositoryFileSystem(propertiesBooter,
                                             repository,
                                             storageProvider.getFileSystem(),
                                             providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public MavenFileSystem mavenRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                     Repository repository,
                                                     FileSystem storageFileSystem,
                                                     LayoutFileSystemProvider provider)
    {
        return new MavenFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

}
