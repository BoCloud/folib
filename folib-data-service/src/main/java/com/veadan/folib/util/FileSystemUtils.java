package com.veadan.folib.util;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

import org.springframework.util.Assert;

/**
 * @author veadan
 * @see org.springframework.util.FileSystemUtils
 */
public final class FileSystemUtils
{

    private FileSystemUtils()
    {
    }

    /**
     * This method is almost a mirror of springframework implementation.
     * However, it contains a workaround for {@link ProviderMismatchException}
     * when I trying to .resolve() a Path against another type of Path
     *
     * @see org.springframework.util.FileSystemUtils#copyRecursively(java.nio.file.Path, java.nio.file.Path)
     */
    public static void copyRecursively(Path src,
                                       Path dest)
            throws IOException
    {
        Assert.notNull(src, "Source Path must not be null");
        Assert.notNull(dest, "Destination Path must not be null");
        BasicFileAttributes srcAttr = Files.readAttributes(src, BasicFileAttributes.class);

        if (srcAttr.isDirectory())
        {
            Files.walkFileTree(src, new SimpleFileVisitor<Path>()
            {
                @Override
                public FileVisitResult preVisitDirectory(Path dir,
                                                         BasicFileAttributes attrs)
                        throws IOException
                {
                    Files.createDirectories(dest.resolve(src.relativize(dir).toString()));
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException
                {
                    Files.copy(file, dest.resolve(src.relativize(file).toString()),
                               StandardCopyOption.REPLACE_EXISTING);
                    return FileVisitResult.CONTINUE;
                }
            });
        }
        else if (srcAttr.isRegularFile())
        {
            Files.copy(src, dest);
        }
        else
        {
            throw new IllegalArgumentException("Source File must denote a directory or file");
        }
    }
}
