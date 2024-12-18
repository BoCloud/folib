package com.veadan.folib.security.resolvepath;

import com.veadan.folib.security.enums.ResolvePathTypeEnum;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class PubResolvePathProvider implements ResolvePathProvider {

    public static final String API_PACKAGES = "api/packages/";

    public static final String PACKAGES = "packages/";

    public static final String VERSIONS = "/versions/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.PUB.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.PUB.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        if (relativePath.contains(API_PACKAGES)) {
            relativePath = relativePath.replace(API_PACKAGES, "");
        } else if (relativePath.contains(PACKAGES) && relativePath.contains(VERSIONS)) {
            relativePath = relativePath.replace("packages/", "").replace("/versions/", "/");
        }
        return relativePath;
    }
}
