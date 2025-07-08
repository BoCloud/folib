package com.folib.config;

import com.folib.booters.StorageBooter;
import com.folib.storage.checksum.ChecksumCacheManager;
import com.folib.storage.validation.ArtifactCoordinatesValidator;

import jakarta.inject.Inject;
import java.util.LinkedHashSet;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;

@Configuration
@ComponentScan({ "com.folib.artifact",
                 "com.folib.configuration",
                 "com.folib.io",
                 "com.folib.providers",
                 "com.folib.services",
                 "com.folib.storage",
                 "com.folib.yaml",
                 "com.folib.dependency",
                 "com.folib.domain",
                 "com.folib.booters",
                 "com.folib.component"
})
@EnableAsync
public class StorageApiConfig
{

    @Inject
    private List<ArtifactCoordinatesValidator> versionValidators;

    @Bean
    ChecksumCacheManager checksumCacheManager()
    {
        ChecksumCacheManager checksumCacheManager = new ChecksumCacheManager();
        checksumCacheManager.setCachedChecksumExpiredCheckInterval(300000);
        checksumCacheManager.setCachedChecksumLifetime(60000);

        return checksumCacheManager;
    }

    @Bean
    LinkedHashSet<ArtifactCoordinatesValidator> versionValidators()
    {
        return new LinkedHashSet<>(versionValidators);
    }


    @Bean
    StorageBooter storageBooter()
    {
        return new StorageBooter();
    }

}
