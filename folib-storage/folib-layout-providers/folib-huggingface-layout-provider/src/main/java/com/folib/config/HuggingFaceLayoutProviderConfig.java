package com.folib.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.layout.HuggingFaceFileSystem;
import com.folib.providers.layout.HuggingFaceFileSystemProvider;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Qualifier;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Configuration
@ComponentScan({"com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage"})
public class HuggingFaceLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." +HuggingFaceLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." + HuggingFaceLayoutProvider.ALIAS;

    @Bean
    @HuggingFaceObjectMapper
    public ObjectMapper huggingFaceObjectMapper() {
        return HuggingFaceJacksonMapperFactory.createObjectMapper();
    }

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory gitLfsRepositoryFileSystemProviderFactory(StorageProviderRegistry
                                                                                             storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());
            LayoutFileSystemProvider result = huggingFaceFileFileSystemProvider(storageProvider.getFileSystemProvider());
            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public HuggingFaceFileSystemProvider huggingFaceFileFileSystemProvider(FileSystemProvider provider) {
        return new HuggingFaceFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory gitLfsRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                     StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory = gitLfsRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return huggingFaceFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public HuggingFaceFileSystem huggingFaceFileSystem(PropertiesBooter propertiesBooter,
                                                            Repository repository,
                                                            FileSystem storageFileSystem,
                                                            LayoutFileSystemProvider provider) {
        return new HuggingFaceFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Documented
    @Qualifier
    @Retention(RUNTIME)
    public static @interface HuggingFaceObjectMapper {

    }

}
