package com.folib.security.resolvepath;

import com.folib.security.enums.ResolvePathTypeEnum;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class ConanResolvePathProvider implements ResolvePathProvider {

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.CONAN.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.CONAN.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        return relativePath;
    }
}
