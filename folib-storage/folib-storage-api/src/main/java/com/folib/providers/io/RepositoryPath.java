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
package com.folib.providers.io;

import com.folib.domain.Artifact;
import com.folib.io.ProxyPathInvocationHandler;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.PathUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import javax.ws.rs.core.MultivaluedMap;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchEvent.Modifier;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * This implementation decorates storage {@link Path} implementation, which can be an "Cloud Path" or common
 * "File System Path".
 *
 * @author @author veadan
 * @see RepositoryPathResolver
 */
public class RepositoryPath
        implements Path {

    private Path target;

    private LayoutFileSystem fileSystem;

    protected Artifact artifact;

    protected Boolean artifactExist;

    protected Map<RepositoryFileAttributeType, Object> cachedAttributes = new HashMap<>();

    protected URI uri;

    private String targetUrl;

    private Boolean disableRemote;

    private MultivaluedMap<String, Object> headers;

    protected String path;

    private String artifactPath;

    private Long size;

    public Map<String,String>  getExtAttribute() {
        return extAttribute;
    }

    public void setExtAttribute(Map<String,String> extAttribute) {
        this.extAttribute = extAttribute;
    }

    protected Map<String,String> extAttribute;

    public RepositoryPath(Path target,
                          LayoutFileSystem fileSystem) {
        this.target = target;
        this.fileSystem = fileSystem;
    }

    public String getStorageId() {
        return getRepository().getStorage().getId();
    }

    public String getRepositoryId() {
        return getRepository().getId();
    }

    public Path getTarget() {
        return target;
    }

    public void setTarget(Path target) {
        this.target = target;
    }

    public Artifact getArtifactEntry() throws IOException {
        return artifact;
    }

    public Boolean getArtifactExist() throws IOException {
        return artifactExist;
    }

    public void setArtifact(Artifact artifact) {
        this.artifact = artifact;
    }

    @Override
    public LayoutFileSystem getFileSystem() {
        return fileSystem;
    }

    public Repository getRepository() {
        return getFileSystem().getRepository();
    }

    @Override
    public boolean isAbsolute() {
        return getTarget().isAbsolute();
    }

    @Override
    public RepositoryPath getRoot() {
        return getFileSystem().getRootDirectory();
    }

    @Override
    public Path getFileName() {
        return getTarget().getFileName();
    }

    @Override
    public RepositoryPath getParent() {
        RepositoryPath parent = wrap(getTarget().getParent());

        validateParent(parent);

        return parent;
    }

    @Override
    public int getNameCount() {
        return getTarget().getNameCount();
    }

    @Override
    public RepositoryPath getName(int index) {
        return wrap(getTarget().getName(index));
    }

    @Override
    public RepositoryPath subpath(int beginIndex,
                                  int endIndex) {
        return wrap(getTarget().subpath(beginIndex, endIndex));
    }

    @Override
    public boolean startsWith(Path other) {
        return getTarget().startsWith(unwrap(other));
    }

    @Override
    public boolean startsWith(String other) {
        return getTarget().startsWith(other);
    }

    @Override
    public boolean endsWith(Path other) {
        return getTarget().endsWith(other);
    }

    @Override
    public boolean endsWith(String other) {
        return getTarget().endsWith(other);
    }

    @Override
    public RepositoryPath normalize() {
        return wrap(getTarget().normalize());
    }

    @Override
    public RepositoryPath resolve(Path other) {
        if (other == null) {
            return this;
        }

        other = unwrap(other);

        validatePathRelativized(other);

        return wrap(getTarget().resolve(other));
    }

    @Override
    public RepositoryPath resolve(String other) {
        if (other == null) {
            return this;
        }
        Map<String,String> extAttribute=null;
        if(other.contains(";")){
            String[] split = other.split(";");
            other=split[0];
            extAttribute=new HashMap<>();
            for (int i = 1; i <split.length ; i++) {
                String item = split[i];
                String[] keyAndValue = item.split("=");

                if(keyAndValue.length==2){
                    extAttribute.put(keyAndValue[0],keyAndValue[1]);
                }
            }
        }
        validateStringPathRelativized(other);
        RepositoryPath wrap = wrap(getTarget().resolve(other));
        wrap.setExtAttribute(extAttribute);
        return wrap ;
    }

    @Override
    public RepositoryPath resolveSibling(Path other) {
        validatePathRelativized(other);

        other = unwrap(other);

        RepositoryPath result = wrap(getTarget().resolveSibling(other));

        validateSibling(result);

        return result;
    }

    protected Path unwrap(Path other) {
        other = other != null && Proxy.isProxyClass(other.getClass())
                ? ((ProxyPathInvocationHandler) Proxy.getInvocationHandler(other)).getTarget()
                : other;

        other = other instanceof RepositoryPath ? ((RepositoryPath) other).getTarget() : other;

        return other;
    }

    @Override
    public RepositoryPath resolveSibling(String other) {
        validateStringPathRelativized(other);

        RepositoryPath result = wrap(getTarget().resolveSibling(other));

        validateSibling(result);

        return result;
    }

    @Override
    public RepositoryPath relativize(Path other) {
        other = unwrap(other);

        return wrap(getTarget().relativize(other));
    }

    /**
     * Returns Path relative to Repository root.
     *
     * @return
     */
    public RepositoryPath relativize() {
        if (!isAbsolute()) {
            return this;
        }

        RepositoryPath result = getFileSystem().getRootDirectory().relativize(this);
        if (result.startsWith(LayoutFileSystem.TEMP)) {
            result = result.subpath(1, result.getNameCount());
        }

        return result;
    }

    @Override
    public URI toUri() {
        if (uri != null) {
            return uri;
        }

        Repository repository = getFileSystem().getRepository();
        Storage storage = repository.getStorage();
        try {
            uri = new URI(StorageFileSystemProvider.FOLIB_SCHEME,
                    null,
                    "/" + storage.getId() + "/" + repository.getId() + "/" +
                            FilenameUtils.separatorsToUnix(relativize().toString()),
                    null);
        } catch (URISyntaxException e) {
            uri = null;
        }
        return uri;
    }

    @Override
    public RepositoryPath toAbsolutePath() {
        if (!isAbsolute()) {
            RepositoryPath result = getFileSystem().getRootDirectory().resolve(this);
            result.artifact = this.artifact;

            return result;
        }

        return this;
    }

    @Override
    public RepositoryPath toRealPath(LinkOption... options)
            throws IOException {
        return wrap(getTarget().toRealPath(options));
    }

    @Override
    @Deprecated
    public File toFile() {
        return getTarget().toFile();
    }

    @Override
    public WatchKey register(WatchService watcher,
                             Kind<?>[] events,
                             Modifier... modifiers)
            throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public WatchKey register(WatchService watcher,
                             Kind<?>... events)
            throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<Path> iterator() {
        return getTarget().iterator();
    }

    @Override
    public int compareTo(Path other) {
        return getTarget().compareTo(unwrap(other));
    }

    public RepositoryPath wrap(Path path) {
        return new RepositoryPath(path, fileSystem);
    }

    @Override
    public String toString() {
        return getTarget().toString();
    }

    @Override
    public boolean equals(Object obj) {
        return getTarget().equals(obj instanceof RepositoryPath ? unwrap((Path) obj) : obj);
    }

    @Override
    public int hashCode() {
        return getTarget().hashCode();
    }

    private void validatePathRelativized(final Path other) {
        if (!PathUtils.isRelativized(unwrap(target), unwrap(other))) {
            throw new RepositoryRelativePathConstructionException();
        }
    }

    private void validateStringPathRelativized(final String other) {
        if (!PathUtils.isRelativized(unwrap(target), other)) {
            throw new RepositoryRelativePathConstructionException();
        }
    }

    private void validateParent(final Path parent) {
        RootRepositoryPath root = getFileSystem().getRootDirectory();
        if (parent.isAbsolute() && !parent.startsWith(root)) {
            throw new RepositoryRelativePathConstructionException();
        }
    }

    private void validateSibling(final Path result) {
        final Path sibling = result;
        final String repositoryRootPath = getRoot().toString(); // String, intentionally
        if (sibling.isAbsolute() && !sibling.startsWith(repositoryRootPath)) {
            throw new PathExceededRootRepositoryPathException();
        }
    }

    public String getTargetUrl() {
        return targetUrl;
    }

    public void setTargetUrl(String targetUrl) {
        this.targetUrl = targetUrl;
    }

    public MultivaluedMap<String, Object> getHeaders() {
        return headers;
    }

    public void setHeaders(MultivaluedMap<String, Object> headers) {
        this.headers = headers;
    }

    public String getArtifactPath() {
        return artifactPath;
    }

    public String getPath() {
        if (StringUtils.isBlank(path)) {
            path = FilenameUtils.separatorsToUnix(relativize().toString());
        }
        return path;
    }

    public String getOriginalPath() {
        if (StringUtils.isBlank(path)) {
            path = relativize().toString();
        }
        return path;
    }

    public void setArtifactPath(String artifactPath) {
        this.artifactPath = artifactPath;
    }

    public Boolean getDisableRemote() {
        return disableRemote;
    }

    public void setDisableRemote(Boolean disableRemote) {
        this.disableRemote = disableRemote;
    }

    public Long getSize() {
        return size;
    }

    public void setSize(Long size) {
        this.size = size;
    }

    public String getName(){
        Path path =getTarget().getName(getTarget().getNameCount()-1);
        return path.getFileName().toString();
    }
}
