/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.config.webdav;

import com.folib.utils.PathUtils;
import io.milton.http.Auth;
import io.milton.http.FileItem;
import io.milton.http.Range;
import io.milton.http.Request;
import io.milton.http.exceptions.BadRequestException;
import io.milton.http.exceptions.ConflictException;
import io.milton.http.exceptions.NotAuthorizedException;
import io.milton.resource.CollectionResource;
import io.milton.resource.FileResource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Date;
import java.util.Map;

/**
 * @author veadan
 * @since 2025-03-09 16:08
 */
@Slf4j
public class FolibFileResource implements FileResource {
    private final String path;
    private final FileStorageService fileStorageService;

    public FolibFileResource(String path, FileStorageService fileStorageService) {
        this.path = path;
        this.fileStorageService = fileStorageService;
    }

    protected String getPath() {
        return this.path;
    }

    @Override
    public void sendContent(OutputStream outputStream, Range range, Map<String, String> params, String contentType) throws IOException {
        try (InputStream fileInputStream = this.fileStorageService.getFileInputStream(this.path)) {
            if (range != null) {
                int start = range.getStart().intValue();
                int finish = range.getFinish().intValue();
                int length = finish - start + 1;
                // 跳过流中的起始字节
                long skipped = fileInputStream.skip(start);
                if (skipped != start) {
                    throw new IOException("Unable to skip to the requested range start: " + start);
                }
                // 传输指定长度的字节
                byte[] buffer = new byte[8192]; // 8KB 缓冲区
                long remaining = length;
                int bytesRead;
                while (remaining > 0 && (bytesRead = fileInputStream.read(buffer, 0, (int) Math.min(buffer.length, remaining))) != -1) {
                    outputStream.write(buffer, 0, bytesRead);
                    remaining -= bytesRead;
                }
            } else {
                IOUtils.copy(fileInputStream, outputStream);
            }

        }

    }

    @Override
    public Long getMaxAgeSeconds(Auth auth) {
        return 0L;
    }

    @Override
    public String getContentType(String s) {
        return "application/octet-stream";
    }

    @Override
    public Long getContentLength() {
        return this.fileStorageService.getContentLength(path);
    }

    @Override
    public String getUniqueId() {
        return this.path;
    }

    @Override
    public String getName() {
        return PathUtils.getLastPathElement(path);
    }

    @Override
    public Object authenticate(String user, String password) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth != null && auth.isAuthenticated()) {
            return auth.getName(); // 返回认证对象给 Milton
        }
        return null; // 未认证;
    }

    @Override
    public boolean authorise(Request request, Request.Method method, Auth auth) {
        return true;
    }

    @Override
    public String getRealm() {
        return "folib";
    }

    @Override
    public Date getModifiedDate() {
        return fileStorageService.getModifiedDate(this.path);
    }

    @Override
    public String checkRedirect(Request request) throws NotAuthorizedException, BadRequestException {
        return "";
    }

    @Override
    public Date getCreateDate() {
        return fileStorageService.getModifiedDate(this.path);
    }

    @Override
    public void copyTo(CollectionResource collectionResource, String name) throws NotAuthorizedException, BadRequestException, ConflictException {
        log.info("Copying {} to {} with name {}", path, collectionResource.getName(), name);
        String destPath = collectionResource instanceof FolibFolderResource ?
                ((FolibFolderResource) collectionResource).getPath() + (collectionResource.getName().isEmpty() ? "" : "/") + name : name;
        if (fileStorageService.exists(destPath)) {
            return;
        }
        fileStorageService.copyFile(path, destPath);
    }

    @Override
    public void delete() throws NotAuthorizedException, ConflictException, BadRequestException {
        try {
            this.fileStorageService.deleteFile(this.path);
        } catch (IOException e) {
            throw new BadRequestException(e.getLocalizedMessage());
        }
    }

    @Override
    public void moveTo(CollectionResource collectionResource, String name) throws ConflictException, NotAuthorizedException, BadRequestException {
        log.info("move {} to {} with name {}", path, collectionResource.getName(), name);
        String destPath = collectionResource instanceof FolibFolderResource ?
                ((FolibFolderResource) collectionResource).getPath() + (collectionResource.getName().isEmpty() ? "" : "/") + name : name;
        if (fileStorageService.exists(destPath)) {
            return;
        }
        fileStorageService.moveFile(path, destPath);

    }

    @Override
    public String processForm(Map<String, String> map, Map<String, FileItem> map1) throws BadRequestException, NotAuthorizedException, ConflictException {
        return "";
    }
}
