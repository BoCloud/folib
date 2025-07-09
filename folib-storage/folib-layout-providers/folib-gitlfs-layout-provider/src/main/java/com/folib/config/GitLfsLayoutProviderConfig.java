package com.folib.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.GitFlsFileSystem;
import com.folib.providers.GitFlsFileSystemProvider;
import com.folib.providers.GitLfsLayoutProvider;
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
public class GitLfsLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + GitLfsLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            GitLfsLayoutProvider.ALIAS;

    @Bean
    @GitLfsObjectMapper
    public ObjectMapper gitLfsObjectMapper() {
        return GitLfsJacksonMapperFactory.createObjectMapper();
    }

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory gitLfsRepositoryFileSystemProviderFactory(StorageProviderRegistry
                                                                                             storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = gitLfsFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public GitFlsFileSystemProvider gitLfsFileSystemProvider(FileSystemProvider provider) {
        return new GitFlsFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory gitLfsRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                     StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory = gitLfsRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return gitlfsRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public GitFlsFileSystem gitlfsRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                       Repository repository,
                                                       FileSystem storageFileSystem,
                                                       LayoutFileSystemProvider provider) {
        return new GitFlsFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Documented
    @Qualifier
    @Retention(RUNTIME)
    public static @interface GitLfsObjectMapper {

    }

}