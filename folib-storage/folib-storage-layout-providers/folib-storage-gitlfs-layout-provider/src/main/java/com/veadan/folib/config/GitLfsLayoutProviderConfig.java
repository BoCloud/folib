package com.veadan.folib.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.GitFlsFileSystem;
import com.veadan.folib.providers.layout.GitFlsFileSystemProvider;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;
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
@ComponentScan({"com.veadan.folib.repository",
        "com.veadan.folib.providers",
        "com.veadan.folib.services",
        "com.veadan.folib.storage",
        "com.veadan.folib.gitls.service"})
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