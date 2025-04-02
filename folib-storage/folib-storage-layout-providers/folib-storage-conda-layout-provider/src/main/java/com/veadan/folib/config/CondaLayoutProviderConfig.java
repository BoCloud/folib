package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.CondaFileSystem;
import com.veadan.folib.providers.layout.CondaFileSystemProvider;
import com.veadan.folib.providers.layout.CondaLayoutProvider;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.providers.storage.StorageProviderRegistry;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author LingengMa
 * @date 2025-04-02 13:55
 */
@Configuration
@ComponentScan({"com.veadan.folib.repository",
        "com.veadan.folib.providers",
        "com.veadan.folib.services",
        "com.veadan.folib.storage"})
public class CondaLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + CondaLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            CondaLayoutProvider.ALIAS;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory CondaRepositoryFileSystemProviderFactory(StorageProviderRegistry storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = CondaFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public CondaFileSystemProvider CondaFileSystemProvider(FileSystemProvider provider) {
        return new CondaFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory CondaRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                 StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory =
                CondaRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return CondaRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public CondaFileSystem CondaRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                     Repository repository,
                                                     FileSystem storageFileSystem,
                                                     LayoutFileSystemProvider provider) {
        return new CondaFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }
}
