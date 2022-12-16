package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.PhpFileSystem;
import com.veadan.folib.providers.layout.PhpFileSystemProvider;
import com.veadan.folib.providers.layout.PhpLayoutProvider;
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
 * @author leipenghui
 */
@Configuration
@ComponentScan({"com.veadan.folib.repository",
        "com.veadan.folib.providers",
        "com.veadan.folib.services",
        "com.veadan.folib.storage"})
public class PhpLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + PhpLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            PhpLayoutProvider.ALIAS;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory phpRepositoryFileSystemProviderFactory(StorageProviderRegistry storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = phpFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public PhpFileSystemProvider phpFileSystemProvider(FileSystemProvider provider) {
        return new PhpFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory phpRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                  StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory = phpRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return phpRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public PhpFileSystem phpRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                 Repository repository,
                                                 FileSystem storageFileSystem,
                                                 LayoutFileSystemProvider provider) {
        return new PhpFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }
}
