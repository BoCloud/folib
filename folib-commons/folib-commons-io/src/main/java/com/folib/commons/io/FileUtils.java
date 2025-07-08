package com.folib.commons.io;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * @author veadan
 */
public class FileUtils
{

    private FileUtils()
    {
    }

    static void moveDirectory(Path srcPath,
                              Path destPath)
            throws IOException
    {
        if (!Files.isDirectory(srcPath))
        {
            throw new IOException(srcPath.toAbsolutePath().toString() + " is not a directory!");
        }

        if (Files.notExists(destPath))
        {
            Files.createDirectories(destPath);
        }

        Files.move(srcPath, destPath, StandardCopyOption.REPLACE_EXISTING);
    }

}
