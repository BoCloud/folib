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


import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.output.ProxyOutputStream;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.FileSystemUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.*;
import java.nio.file.DirectoryStream.Filter;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.FileAttributeView;
import java.nio.file.spi.FileSystemProvider;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This class decorates storage {@link FileSystemProvider}.
 * <br>
 * Note that almost all {@link Path} operations in this implementation must
 * delegate invocations into {@link Files} utility class with {@link RepositoryPath}'s target as parameters.
 * <p>
 * TODO: we need a proper implementation against Service Provider Interface (SPI) specification
 *
 * @author @author veadan
 */
@Slf4j
public abstract class StorageFileSystemProvider
        extends FileSystemProvider {

    public static final String FOLIB_SCHEME = "folib";

    private static final Logger logger = LoggerFactory.getLogger(StorageFileSystemProvider.class);

    private FileSystemProvider target;

    public StorageFileSystemProvider(FileSystemProvider target) {
        super();
        this.target = target;
    }

    @Override
    public String getScheme() {
        return FOLIB_SCHEME;
    }

    @Override
    public FileSystem newFileSystem(URI uri,
                                    Map<String, ?> env)
            throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public FileSystem getFileSystem(URI uri) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Path getPath(URI uri) {
        throw new UnsupportedOperationException();
    }

    @Override
    public SeekableByteChannel newByteChannel(Path path,
                                              Set<? extends OpenOption> options,
                                              FileAttribute<?>... attrs)
            throws IOException {
        return getTarget().newByteChannel(unwrap(path), options, attrs);
    }

    @Override
    public FileChannel newFileChannel(Path path,
                                      Set<? extends OpenOption> options,
                                      FileAttribute<?>... attrs)
            throws IOException {
        return getTarget().newFileChannel(unwrap(path), options, attrs);
    }

    @Override
    public DirectoryStream<Path> newDirectoryStream(Path dir,
                                                    Filter<? super Path> filter)
            throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) dir;
        Path root = repositoryPath.getFileSystem().getRootDirectory();
        DirectoryStream<Path> directoryStream = getTarget().newDirectoryStream(unwrap(dir), new LayoutDirectoryStreamFilter(unwrap(root), filter));

        return new DirectoryStream<Path>() {

            @Override
            public void close()
                    throws IOException {
                directoryStream.close();
            }

            @Override
            public Iterator<Path> iterator() {
                return createRepositoryDsIterator(repositoryPath.getFileSystem(), directoryStream.iterator());
            }

        };
    }

    @Override
    public InputStream newInputStream(Path path, OpenOption... options) throws IOException {
        return target.newInputStream(unwrap(path), options);
    }

    protected Iterator<Path> createRepositoryDsIterator(LayoutFileSystem rfs,
                                                        Iterator<Path> iterator) {

        return new Iterator<Path>() {

            @Override
            public boolean hasNext() {
                return iterator.hasNext();
            }

            @Override
            public Path next() {
                return new RepositoryPath(iterator.next(), rfs);
            }

            @Override
            public void remove() {
                iterator.remove();
            }

            @Override
            public void forEachRemaining(Consumer<? super Path> action) {
                iterator.forEachRemaining(action);
            }

        };
    }

    @Override
    public void createDirectory(Path dir,
                                FileAttribute<?>... attrs)
            throws IOException {
        getTarget().createDirectory(unwrap(dir), attrs);
    }

    @Override
    public void delete(Path path)
            throws IOException {
        delete(path, false);
    }

    public void delete(Path path,
                       boolean force)
            throws IOException {
        if (!(path instanceof RepositoryPath)) {
            Files.delete(path);

            return;
        }

        RepositoryPath repositoryPath = (RepositoryPath) path;
        if (!Files.isDirectory(repositoryPath)) {
            doDeletePath(repositoryPath, force, true);

            return;
        }

        RootRepositoryPath root = repositoryPath.getFileSystem().getRootDirectory();
        recursiveDeleteExceptRoot(repositoryPath, force);
        if (!root.equals(path)) {
            return;
        }

        logger.debug("Deleting hidden folders for [{}]", path);

        FileSystemUtils.deleteRecursively(unwrap(root).resolve(LayoutFileSystem.TEMP));
        Files.delete(unwrap(root));

        logger.debug("Hidden folders deleted [{}]", path);

    }

    protected void recursiveDeleteExceptRoot(RepositoryPath repositoryPath,
                                             boolean force)
            throws IOException {
        RootRepositoryPath root = repositoryPath.getFileSystem().getRootDirectory();

        Files.walkFileTree(repositoryPath, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
                log.warn(exc.getMessage());
                // 目录或文件已删除，继续遍历
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult visitFile(Path file,
                                             BasicFileAttributes attrs)
                    throws IOException {
                // Checksum files will be deleted during directory walking
                doDeletePath((RepositoryPath) file, force, false);

                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir,
                                                      IOException exc)
                    throws IOException {
                if (root.equals(dir)) {
                    return FileVisitResult.CONTINUE;
                }

                try {
                    Files.delete(unwrap(dir));
                } catch (DirectoryNotEmptyException e) {
                    try (Stream<Path> pathStream = Files.list(unwrap(dir))) {
                        String message = pathStream
                                .map(p -> p.getFileName().toString())
                                .reduce((p1,
                                         p2) -> String.format("%s%n%s", p1, p2))
                                .get();
                        throw new IOException(message, e);
                    }
                }

                return FileVisitResult.CONTINUE;
            }
        });
    }

    protected void doDeletePath(RepositoryPath repositoryPath,
                                boolean force,
                                boolean deleteChecksum)
            throws IOException {
        doDeletePath(repositoryPath, force);

        if (!deleteChecksum) {
            return;
        }

        for (RepositoryPath checksumPath : resolveChecksumPathMap(repositoryPath).values()) {
            if (!Files.exists(unwrap(checksumPath))) {
                continue;
            }
            doDeletePath(checksumPath, force);
        }
    }

    public Map<String, RepositoryPath> resolveChecksumPathMap(RepositoryPath repositoryPath) {
        Map<String, RepositoryPath> result = new HashMap<>();
        for (String digestAlgorithm : repositoryPath.getFileSystem().getDigestAlgorithmSet()) {
            // it creates Checksum file extension name form Digest algorithm
            // name: SHA-1->sha1
            String extension = digestAlgorithm.replaceAll("-", "").toLowerCase();
            result.put(digestAlgorithm, repositoryPath.resolveSibling(repositoryPath.getFileName() + "." + extension));
        }
        return result;
    }

    protected void doDeletePath(RepositoryPath repositoryPath,
                                boolean force)
            throws IOException {
        Files.deleteIfExists(repositoryPath.getTarget());
    }

    public RepositoryPath moveFromTemporaryDirectory(TempRepositoryPath tempPath)
            throws IOException {
        long startTime = System.currentTimeMillis();
        logger.debug("Moving [{}]", tempPath.getTarget());
        RepositoryPath path = tempPath.getTempTarget();

        if (!Files.exists(tempPath.getTarget())) {
            throw new IOException(String.format("[%s] target for [%s] don't exists!", TempRepositoryPath.class.getSimpleName(), tempPath));
        }

        if (!Files.exists(unwrap(path).getParent())) {
            Files.createDirectories(unwrap(path).getParent());
        }
        if (Files.exists(path.getTarget())) {
            Files.delete(path.getTarget());
        }

        Files.move(tempPath.getTarget(), path.getTarget(), StandardCopyOption.ATOMIC_MOVE);


        logger.debug("Moving finished [{}] task time [{}] ms", tempPath.getTarget(), System.currentTimeMillis() - startTime);
        //path.artifactEntry = tempPath.artifactEntry;

        return path;
    }

    public void deleteEmptyDirectory(RepositoryPath repositoryPath)
            throws IOException {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath) || !Files.isDirectory(repositoryPath)) {
            return;
        }
        RootRepositoryPath root = repositoryPath.getFileSystem().getRootDirectory();
        FileSystemUtils.deleteRecursively(unwrap(root).resolve(LayoutFileSystem.TEMP));
    }

    private void deleteEmptyPath(String storageId, String repositoryId, RepositoryPath repositoryPath) {
        try {
            if (Files.exists(repositoryPath) && RepositoryFiles.canDeleteArtifactMetadata(repositoryPath)) {
                Files.deleteIfExists(repositoryPath);
                log.info("Empty path storageId [{}] repositoryId [{}] path [{}] do delete", storageId, repositoryId, repositoryPath.toString());
            }
        } catch (Exception ex) {
            log.error("Empty path storageId [{}] repositoryId [{}] path [{}] error [{}]", storageId, repositoryId, repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    private void deleteEmptyDirectory(String storageId, String repositoryId, RepositoryPath repositoryPath) {
        try {
            if (Files.exists(repositoryPath) && !Files.isSameFile(repositoryPath.getRoot(), repositoryPath) && RepositoryFiles.isDirectoryEmpty(repositoryPath)) {
                Files.deleteIfExists(repositoryPath);
                log.info("Empty directory storageId [{}] repositoryId [{}] dir path [{}] do delete", storageId, repositoryId, repositoryPath.toString());
            }
        } catch (Exception ex) {
            log.error("Empty directory storageId [{}] repositoryId [{}] dir path [{}] error [{}]", storageId, repositoryId, repositoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    protected static RepositoryPath rebase(RepositoryPath source,
                                           RepositoryPath targetBase) {
        String sourceRelative = source.getRoot().relativize(source.getTarget()).toString();

        // XXX[SBESPALOV]: We try to convert path from source to target
        // FileSystem and need to check this on different
        // Storage types.
        // Note that this is only draft implementation, and probably in the
        // future we will need something like separate
        // `FileSystemPathConverter` to convert Paths from one FileSystem to
        // another. Such a `FileSystemPathConverter`
        // can be provided by the `RepositoryFileSystem` instance.

        String sTargetPath = sourceRelative;

        return targetBase.resolve(sTargetPath).toAbsolutePath();
    }

    @Override
    public OutputStream newOutputStream(Path path,
                                        OpenOption... options)
            throws IOException {
        TempRepositoryPath temp = RepositoryFiles.temporary((RepositoryPath) path);
//        return new TempOutputStream(temp, options);
        return new ProxyOutputStream(StorageFileSystemProvider.super.newOutputStream(unwrap(path), options));
    }

    @Override
    public void copy(Path source,
                     Path target,
                     CopyOption... options)
            throws IOException {
        getTarget().copy(unwrap(source), unwrap(target), options);
    }

    @Override
    public void move(Path source,
                     Path target,
                     CopyOption... options)
            throws IOException {
        getTarget().move(unwrap(source), unwrap(target), options);
    }

    @Override
    public boolean isSameFile(Path path,
                              Path path2)
            throws IOException {
        return getTarget().isSameFile(unwrap(path), unwrap(path2));
    }

    @Override
    public boolean isHidden(Path path)
            throws IOException {
        return getTarget().isHidden(unwrap(path));
    }

    @Override
    public FileStore getFileStore(Path path)
            throws IOException {
        return getTarget().getFileStore(unwrap(path));
    }

    @Override
    public void checkAccess(Path path,
                            AccessMode... modes)
            throws IOException {
        getTarget().checkAccess(unwrap(path), modes);
    }

    @Override
    public <V extends FileAttributeView> V getFileAttributeView(Path path,
                                                                Class<V> type,
                                                                LinkOption... options) {
        return getTarget().getFileAttributeView(unwrap(path), type, options);
    }

    @Override
    public <A extends BasicFileAttributes> A readAttributes(Path path,
                                                            Class<A> type,
                                                            LinkOption... options)
            throws IOException {
        if (RepositoryFileAttributes.class.isAssignableFrom(type) && !RepositoryPath.class.isInstance(path)) {
            throw new IOException(String.format("Requested path is not [%s].", RepositoryPath.class.getSimpleName()));
        }

        BasicFileAttributes targetAttributes = getTarget().readAttributes(unwrap(path),
                BasicFileAttributes.class,
                options);
        if (!RepositoryFileAttributes.class.isAssignableFrom(type)) {
            return (A) targetAttributes;
        }

        RepositoryFileAttributes repositoryFileAttributes = new RepositoryFileAttributes(targetAttributes,
                getRepositoryFileAttributes((RepositoryPath) path,
                        RepositoryFiles.parseAttributes("*")
                                .toArray(new RepositoryFileAttributeType[]{})));

        return (A) repositoryFileAttributes;
    }

    @Override
    public Map<String, Object> readAttributes(Path path,
                                              String attributes,
                                              LinkOption... options)
            throws IOException {
        if (!RepositoryPath.class.isInstance(path)) {
            return getTarget().readAttributes(path, attributes, options);
        }
        RepositoryPath repositoryPath = (RepositoryPath) path;

        Map<String, Object> result = new HashMap<>();
        String attributes1 = attributes.equals("*") ? "*" : Arrays.stream(attributes.split(",")).filter(d -> d.startsWith(FOLIB_SCHEME)).collect(Collectors.joining(","));
        String attributes2 = attributes.equals("*") ? "*" : Arrays.stream(attributes.split(",")).filter(d -> !d.startsWith(FOLIB_SCHEME)).collect(Collectors.joining(","));

        if (!attributes2.isEmpty()) {
            result.putAll(getTarget().readAttributes(unwrap(path), attributes2, options));
        }
        //if (!attributes.startsWith(FOLIB_SCHEME)) {
        // result.putAll(getTarget().readAttributes(unwrap(path), attributes2, options));
        //if (!attributes.equals("*")) {
        //    return result;
        //}
        //}

        Set<RepositoryFileAttributeType> targetRepositoryAttributes = new HashSet<>(
                RepositoryFiles.parseAttributes(attributes1));

        final Map<RepositoryFileAttributeType, Object> repositoryFileAttributes = new HashMap<>(targetRepositoryAttributes.size() * 2);
        for (Iterator<RepositoryFileAttributeType> iterator = targetRepositoryAttributes.iterator(); iterator.hasNext(); ) {
            RepositoryFileAttributeType repositoryFileAttributeType = iterator.next();
            Optional.ofNullable(repositoryPath.cachedAttributes.get(repositoryFileAttributeType))
                    .ifPresent(v -> {
                        repositoryFileAttributes.put(repositoryFileAttributeType, v);
                        iterator.remove();
                    });

        }
        if (!targetRepositoryAttributes.isEmpty()) {
            Map<RepositoryFileAttributeType, Object> newAttributes = getRepositoryFileAttributes(repositoryPath,
                    targetRepositoryAttributes.toArray(new RepositoryFileAttributeType[targetRepositoryAttributes.size()]));
            newAttributes.forEach((key, value) -> {
                repositoryFileAttributes.put(key,
                        value);

                repositoryPath.cachedAttributes.put(key,
                        value);
            });
        }

        result.putAll(repositoryFileAttributes.entrySet()
                .stream()
                .collect(Collectors.toMap(e -> e.getKey()
                                .getName(),
                        Map.Entry::getValue)));
        return result;
    }

    protected abstract Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryRelativePath,
                                                                                            RepositoryFileAttributeType... attributeTypes)
            throws IOException;

    @Override
    public void setAttribute(Path path,
                             String attribute,
                             Object value,
                             LinkOption... options)
            throws IOException {
        getTarget().setAttribute(unwrap(path), attribute, value, options);
    }

    protected Path unwrap(Path path) {
        return path instanceof RepositoryPath ? ((RepositoryPath) path).getTarget() : path;
    }

    public FileSystemProvider getTarget() {
        return target;
    }

    public static class MoveDirectoryVisitor
            extends SimpleFileVisitor<Path> {

        private final Path fromPath;
        private final Path toPath;
        private final CopyOption copyOption;

        public MoveDirectoryVisitor(Path fromPath,
                                    Path toPath,
                                    CopyOption copyOption) {
            this.fromPath = fromPath;
            this.toPath = toPath;
            this.copyOption = copyOption;
        }

        @Override
        public FileVisitResult preVisitDirectory(Path dir,
                                                 BasicFileAttributes attrs)
                throws IOException {
            Path targetPath = toPath.resolve(fromPath.relativize(dir));
            if (!Files.exists(targetPath)) {
                Files.createDirectory(targetPath);
            }

            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult visitFile(Path file,
                                         BasicFileAttributes attrs)
                throws IOException {
            Files.move(file, toPath.resolve(fromPath.relativize(file)), copyOption);

            return FileVisitResult.CONTINUE;
        }
    }


    private class TempOutputStream extends ProxyOutputStream {

        private TempRepositoryPath path;

        public TempOutputStream(TempRepositoryPath path,
                                OpenOption... options)
                throws IOException {
            super(StorageFileSystemProvider.super.newOutputStream(unwrap(path), options));

            this.path = path;
        }

        @Override
        public void close()
                throws IOException {
            super.close();

            try {
                moveFromTemporaryDirectory(path);
            } finally {
                if (Files.exists(path)) {
                    Files.deleteIfExists(path.getTarget());
                }
            }
        }

    }

    private static class LayoutDirectoryStreamFilter implements Filter<Path> {

        private final Filter<? super Path> delegate;
        private final Path root;

        public LayoutDirectoryStreamFilter(Path root, Filter<? super Path> delegate) {
            this.delegate = delegate;
            this.root = root;
        }

        @Override
        public boolean accept(Path p)
                throws IOException {
            if (p.isAbsolute()
                    && !p.startsWith(root.resolve(LayoutFileSystem.TEMP))) {
                return delegate == null ? true : delegate.accept(p);
            }

            return false;
        }

    }

}
