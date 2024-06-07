package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.PubFileSystem;
import com.veadan.folib.providers.layout.PubFileSystemProvider;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.npm.metadata.jackson.NpmJacksonMapperFactory;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import javax.inject.Qualifier;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Configuration
@ComponentScan({ "com.veadan.folib.dependency.snippet",
                 "com.veadan.folib.repository",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage",
})
public class PubLayoutProviderConfig
{

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + PubLayoutProvider.ALIAS;
    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory."
            + PubLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;

    @Bean
    @NpmObjectMapper
    public ObjectMapper npmJacksonMapper()
    {
        return NpmJacksonMapperFactory.createObjectMapper();
    }

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory npmRepositoryFileSystemProviderFactory()
    {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = npmFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public PubFileSystemProvider npmFileSystemProvider(FileSystemProvider provider)
    {
        return new PubFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory npmRepositoryFileSystemFactory(PropertiesBooter propertiesBooter)
    {
        LayoutFileSystemProviderFactory providerFactory = npmRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return npmRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                                           providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public PubFileSystem npmRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                 Repository repository,
                                                 FileSystem storageFileSystem,
                                                 LayoutFileSystemProvider provider)
    {
        return new PubFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Documented
    @Qualifier
    @Retention(RUNTIME)
    public static @interface NpmObjectMapper
    {

    }

}
