package com.veadan.folib.security.resolvepath;

import com.veadan.folib.security.enums.ResolvePathTypeEnum;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class HelmResolvePathProvider implements ResolvePathProvider {

    public static final String API = "api/";

    public static final String STORAGE = "storages/";

    public static final String STORAGE_PREFIX = "/storages";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.HELM.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.HELM.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        if (relativePath.startsWith(API)) {
            relativePath = relativePath.replace(API, STORAGE);
        } else if (!relativePath.startsWith(STORAGE) && !relativePath.startsWith(STORAGE_PREFIX)) {
            relativePath = STORAGE + relativePath;
        }
        return relativePath;
    }
}
