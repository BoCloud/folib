package com.folib.config;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.layout.CjpmFileSystem;
import com.folib.providers.layout.CjpmFileSystemProvider;
import com.folib.providers.layout.CjpmLayoutProvider;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;


@Configuration
@ComponentScan({"com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage"})
public class CjpmLayoutProviderConfig
{

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + CjpmLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." + CjpmLayoutProvider.ALIAS;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory rawRepositoryFileSystemProviderFactory(StorageProviderRegistry storageProviderRegistry)
    {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = cjpmFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public CjpmFileSystemProvider cjpmFileSystemProvider(FileSystemProvider provider)
    {
        return new CjpmFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory cjpmRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                   StorageProviderRegistry storageProviderRegistry)
    {
        LayoutFileSystemProviderFactory providerFactory = rawRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return cjpmRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public CjpmFileSystem cjpmRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                   Repository repository,
                                                   FileSystem storageFileSystem,
                                                   LayoutFileSystemProvider provider)
    {
        return new CjpmFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

}
