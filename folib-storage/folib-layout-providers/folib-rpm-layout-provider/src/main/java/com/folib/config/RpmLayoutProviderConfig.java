package com.folib.config;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.folib.booters.PropertiesBooter;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.RpmFileSystem;
import com.folib.providers.RpmFileSystemProvider;
import com.folib.providers.RpmLayoutProvider;
import com.folib.npm.metadata.*;
import com.folib.npm.metadata.jackson.*;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import com.folib.npm.metadata.Bugs;
import com.folib.npm.metadata.Engines;
import com.folib.npm.metadata.Person;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;
import javax.inject.Qualifier;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Configuration
@ComponentScan({ "com.folib.configuration",
        "com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage",
})
public class RpmLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + RpmLayoutProvider.ALIAS;
    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory."
            + RpmLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;

    @Bean
    @RpmObjectMapper
    public ObjectMapper rpmJacksonMapper()
    {
        ObjectMapper objectMapper = (new ObjectMapper()).enable(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY).enable(DeserializationFeature.UNWRAP_SINGLE_VALUE_ARRAYS);
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
        objectMapper.setDateFormat(df);
        SimpleModule module = new SimpleModule();
        module.addDeserializer(com.folib.npm.metadata.Repository.class, new RepositoryDeserializer());
        module.addDeserializer(License.class, new LicenseDeserializer());
        module.addDeserializer(Bin.class, new BinDeserializer());
        module.addDeserializer(Engines.class, new EnginesDeserializer());
        module.addDeserializer(Person.class, new PersonDeserializer());
        module.addDeserializer(Directories.class, new DirectoriesDeserializer());
        module.addDeserializer(Scripts.class, new ScriptsDeserializer());
        module.addDeserializer(Bugs.class, new BugsDeserializer());
        objectMapper.registerModule(module);
        return objectMapper;
    }


    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory rpmRepositoryFileSystemProviderFactory()
    {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return rpmFileSystemProvider(storageProvider.getFileSystemProvider());
        };

    }


    @Bean
    @Scope("prototype")
    public RpmFileSystemProvider rpmFileSystemProvider(FileSystemProvider provider)
    {
        return new RpmFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory rpmRepositoryFileSystemFactory(PropertiesBooter propertiesBooter)
    {
        LayoutFileSystemProviderFactory providerFactory = rpmRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return rpmRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public RpmFileSystem rpmRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                 Repository repository,
                                                 FileSystem storageFileSystem,
                                                 LayoutFileSystemProvider provider)
    {
        return new RpmFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Documented
    @Qualifier
    @Retention(RUNTIME)
    public static @interface RpmObjectMapper
    {

    }

}
