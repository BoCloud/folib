package com.veadan.folib.security.resolvepath;

import com.veadan.folib.constant.GlobalConstants;
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
public class DockerResolvePathProvider implements ResolvePathProvider {

    public static final String MANIFESTS = "/manifests/";

    public static final String BLOBS = "/blobs/";

    public static final String V2 = "/v2/";

    public static final String STORAGES = "/storages/";

    @Inject
    private ResolvePathProviderRegistry resolvePathProviderRegistry;

    @PostConstruct
    @Override
    public void register() {
        resolvePathProviderRegistry.addProvider(ResolvePathTypeEnum.DOCKER.getType(), this);
        log.info("Registered resolve path '{}' with alias '{}'.",
                getClass().getCanonicalName(), ResolvePathTypeEnum.DOCKER.getType());
    }

    @Override
    public String resolvePath(Repository repository, String relativePath) {
        if (StringUtils.isBlank(relativePath)) {
            return "";
        }
        if (relativePath.startsWith(V2) || relativePath.startsWith(STORAGES)) {
            if (relativePath.startsWith(V2)) {
                relativePath = relativePath.replaceFirst(V2, STORAGES);
            }
            if (relativePath.contains(MANIFESTS)) {
                if (!relativePath.contains(GlobalConstants.SHA_256)) {
                    //tag号开头的
                    relativePath = relativePath.replace(MANIFESTS, "/");
                } else {
                    //sha256 manifest文件
                    relativePath = relativePath.replace("manifests/", "manifest/");
                }
            }
        } else {
            if (relativePath.contains(MANIFESTS)) {
                if (!relativePath.contains(GlobalConstants.SHA_256)) {
                    //tag号开头的
                    relativePath = relativePath.replace(MANIFESTS, "/");
                } else {
                    //sha256 manifest文件
                    relativePath = relativePath.substring(relativePath.indexOf("manifests/")).replace("manifests/", "manifest/");
                }
            } else if (relativePath.contains(BLOBS)) {
                //sha256 blob文件
                relativePath = relativePath.substring(relativePath.indexOf("blobs/"));
            }
        }
        return relativePath;
    }
}
