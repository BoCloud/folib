package com.veadan.folib.config;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.layout.providers.CargoFileSystem;
import com.veadan.folib.layout.providers.CargoFileSystemProvider;
import com.veadan.folib.layout.providers.CargoLayoutProvider;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.LayoutFileSystemProviderFactory;
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

@Configuration
@ComponentScan({"com.veadan.folib.repository",
        "com.veadan.folib.providers",
        "com.veadan.folib.services",
        "com.veadan.folib.extractor",
        "com.veadan.folib.storage"})
public class CargoLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + CargoLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            CargoLayoutProvider.ALIAS;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory cargoRepositoryFileSystemProviderFactory(StorageProviderRegistry storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = cargoFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public CargoFileSystemProvider cargoFileSystemProvider(FileSystemProvider provider) {
        return new CargoFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory cargoRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                  StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory = cargoRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return cargoRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public CargoFileSystem cargoRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                   Repository repository,
                                                   FileSystem storageFileSystem,
                                                   LayoutFileSystemProvider provider) {
        return new CargoFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }
}
