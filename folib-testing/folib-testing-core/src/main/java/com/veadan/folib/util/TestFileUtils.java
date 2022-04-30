package com.veadan.folib.util;

import org.carlspring.commons.io.RandomInputStream;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * @author mtodorov
 */
public class TestFileUtils
{

    private TestFileUtils()
    {
    }

    public static void deleteIfExists(File file)
    {
        if (file.exists())
        {
            //noinspection ResultOfMethodCallIgnored
            file.delete();
        }
    }

    public static void generateFile(ZipOutputStream zos,
                                    long bytesSize)
            throws IOException
    {
        generateFile(zos, bytesSize, "file-with-given-size");
    }

    public static void generateFile(ZipOutputStream zos,
                                    long bytesSize,
                                    String name)
            throws IOException
    {
        ZipEntry ze = new ZipEntry(name);
        zos.putNextEntry(ze);

        try (RandomInputStream ris = new RandomInputStream(bytesSize))
        {
            byte[] buffer = new byte[4096];
            int len;
            while ((len = ris.read(buffer)) > 0)
            {
                zos.write(buffer, 0, len);
            }
        }

        zos.closeEntry();
    }

    public static void generateFile(OutputStream bos,
                                    long bytesSize)
            throws IOException
    {
        bos.write("data = \"".getBytes(StandardCharsets.UTF_8));

        OutputStream dataOut = Base64.getEncoder().wrap(bos);
        try (RandomInputStream ris = new RandomInputStream(bytesSize))
        {
            byte[] buffer = new byte[4096];
            int len;
            while ((len = ris.read(buffer)) > 0)
            {
                dataOut.write(buffer, 0, len);
            }
        }

        bos.write("\";".getBytes(StandardCharsets.UTF_8));
    }

    public static void generateFile(File file,
                                    long bytesSize)
            throws IOException
    {
        OutputStream os = new FileOutputStream(file);
        try (RandomInputStream ris = new RandomInputStream(bytesSize))
        {
            byte[] buffer = new byte[4096];
            int len;
            while ((len = ris.read(buffer)) > 0)
            {
                os.write(buffer, 0, len);
            }
        }
    }

}
