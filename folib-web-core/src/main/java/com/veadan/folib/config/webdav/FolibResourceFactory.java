package com.veadan.folib.config.webdav;

import cn.hutool.extra.spring.SpringUtil;
import io.milton.http.ResourceFactory;
import io.milton.http.exceptions.BadRequestException;
import io.milton.http.exceptions.NotAuthorizedException;
import io.milton.resource.Resource;
import lombok.extern.slf4j.Slf4j;

/**
 * @author huayanjun
 * @since 2025-03-09 16:17
 */
@Slf4j
public class FolibResourceFactory implements ResourceFactory {


    private final FileStorageService storageService;

    public FolibResourceFactory() {
        this.storageService = SpringUtil.getBean(FileStorageService.class);
    }

    @Override
    public Resource getResource(String host, String path) throws NotAuthorizedException, BadRequestException {
        String cleanedPath = path.replaceAll("/{2,}", "/")
                .replaceFirst("^/dav/?", "")
                .replaceAll("/$", "");
        log.info("Original: " + path + " -> Cleaned: " + cleanedPath);
        if (!storageService.exists(cleanedPath)) {
            return null;
        }
        return storageService.isDirectory(cleanedPath) ?
                new FolibFolderResource(cleanedPath, storageService) :
                new FolibFileResource(cleanedPath, storageService);

    }
}
