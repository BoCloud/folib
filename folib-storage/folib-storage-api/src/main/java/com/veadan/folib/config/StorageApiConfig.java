package com.veadan.folib.config;

import com.veadan.folib.booters.StorageBooter;
import com.veadan.folib.storage.checksum.ChecksumCacheManager;
import com.veadan.folib.storage.validation.ArtifactCoordinatesValidator;

import javax.inject.Inject;
import java.util.LinkedHashSet;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;

@Configuration
@ComponentScan({ "com.veadan.folib.artifact",
                 "com.veadan.folib.configuration",
                 "com.veadan.folib.io",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage",
                 "com.veadan.folib.yaml",
                 "com.veadan.folib.dependency",
                 "com.veadan.folib.domain",
                 "com.veadan.folib.booters"
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
