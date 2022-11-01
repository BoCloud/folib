package com.veadan.folib.util;

import com.veadan.folib.cloud.storage.s3fs.S3FileSystem;
import com.veadan.folib.cloud.storage.s3fs.S3Path;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * @author veadan
 */
public final class PathUtils
{

    private PathUtils()
    {

    }

    public static boolean isRelativized(final Path base,
                                        final String successor)
    {
        if (base instanceof S3Path) {
            return isRelativized(base, new S3Path((S3FileSystem) base.getFileSystem(), successor));
        }

        return isRelativized(base, Paths.get(successor));
    }

    public static boolean isRelativized(final Path base,
                                        final Path successor)
    {
        Objects.requireNonNull(base);
        Objects.requireNonNull(successor);

        final Path baseNormalized = base.normalize();
        final Path successorNormalized = successor.normalize();
        return baseNormalized.relativize(baseNormalized.resolve(successorNormalized)).equals(successorNormalized);
    }
}
