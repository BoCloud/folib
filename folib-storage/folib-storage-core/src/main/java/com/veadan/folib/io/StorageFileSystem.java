package com.veadan.folib.io;

import java.io.IOException;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.WatchService;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.nio.file.spi.FileSystemProvider;
import java.util.ArrayList;
import java.util.Optional;
import java.util.Set;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Path;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.providers.storage.StorageProvider;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import org.apache.commons.lang3.StringUtils;

/**
 * This class decoretes storage {@link FileSystem} implementation.
 *
 * @author @author veadan
 * 
 * @see StorageProvider
 */
public abstract class StorageFileSystem
        extends FileSystem
{

    private final PropertiesBooter propertiesBooter;
    
    private final FileSystem target;
    
    private final Storage storage;

    public StorageFileSystem(Storage storage, PropertiesBooter propertiesBooter, FileSystem target)
    {
        this.target = target;
        this.storage = storage;
        this.propertiesBooter = propertiesBooter;
    }

    public FileSystem getTarget()
    {
        return target;
    }

    public FileSystemProvider provider()
    {
        return target.provider();
    }

    public void close()
            throws IOException
    {
        target.close();
    }

    public boolean isOpen()
    {
        return target.isOpen();
    }

    public boolean isReadOnly()
    {
        return target.isReadOnly();
    }

    public String getSeparator()
    {
        return target.getSeparator();
    }

    public Iterable<Path> getRootDirectories()
    {
        ArrayList<Path> result = new ArrayList<Path>();
        result.add(getRootDirectory());
        return result;
    }

    public Path getRootDirectory() {
        String basedir = storage.getBasedir();
        if (target instanceof S3FileSystem) {
            ConfigurationManagementService configurationManagementService = SpringUtil.getBean(ConfigurationManagementService.class);
            String globalS3Bucket = configurationManagementService.getConfiguration().getAdvancedConfiguration().getGlobalS3Bucket();
            if (StringUtils.isNotBlank(globalS3Bucket)) {
                globalS3Bucket = GlobalConstants.SEPARATOR + StringUtils.removeEnd(StringUtils.removeStart(globalS3Bucket, GlobalConstants.SEPARATOR), GlobalConstants.SEPARATOR);
                basedir = globalS3Bucket + basedir;
            }
            return new S3Path((S3FileSystem) target, basedir);
        }

        Path storagesRoot = Optional.ofNullable(propertiesBooter.getStorageBooterBasedir())
                                    .filter(p -> !p.trim().isEmpty())
                                    .map(p -> getTarget().getPath(p))
                                    .orElseGet(() -> getTarget().getPath(propertiesBooter.getVaultDirectory(),
                                                                         "/storages"))
                                    .toAbsolutePath()
                                    .normalize();
        return Optional.ofNullable(basedir)
                .filter(p -> !p.trim().isEmpty())
                .map(p -> getTarget().getPath(propertiesBooter.getVaultDirectory() + p).toAbsolutePath().normalize())
                .orElseGet(() -> storagesRoot.resolve(storage.getId())).toAbsolutePath().normalize();
    }

    public Iterable<FileStore> getFileStores()
    {
        return target.getFileStores();
    }

    public Set<String> supportedFileAttributeViews()
    {
        return target.supportedFileAttributeViews();
    }

    public Path getPath(String first,
                        String... more)
    {
        return target.getPath(first, more);
    }

    public PathMatcher getPathMatcher(String syntaxAndPattern)
    {
        return target.getPathMatcher(syntaxAndPattern);
    }

    public UserPrincipalLookupService getUserPrincipalLookupService()
    {
        return target.getUserPrincipalLookupService();
    }

    public WatchService newWatchService()
            throws IOException
    {
        return target.newWatchService();
    }

}
