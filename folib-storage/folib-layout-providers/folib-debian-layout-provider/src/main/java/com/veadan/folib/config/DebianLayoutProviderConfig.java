package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
import com.veadan.folib.providers.layout.DebianFileSystem;
import com.veadan.folib.providers.layout.DebianFileSystemProvider;
import com.veadan.folib.providers.layout.DebianLayoutProvider;
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

/**
 * @author veadan
 */

@Configuration
@ComponentScan("com.veadan.folib")
public class DebianLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + DebianLayoutProvider.ALIAS;
    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." + DebianLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;


    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory debianRepositoryFileSystemProviderFactory() {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());
            return debianFileSystemProvider(storageProvider.getFileSystemProvider());
        };

    }

    @Bean
    @Scope("prototype")
    public DebianFileSystemProvider debianFileSystemProvider(FileSystemProvider provider) {
        return new DebianFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory debianRepositoryFileSystemFactory(PropertiesBooter propertiesBooter) {
        LayoutFileSystemProviderFactory providerFactory = debianRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return debianRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public DebianFileSystem debianRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                       Repository repository,
                                                       FileSystem storageFileSystem,
                                                       LayoutFileSystemProvider provider) {
        return new DebianFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }


}
