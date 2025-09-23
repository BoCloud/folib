package com.folib.nuget.indexer;

import com.folib.nuget.indexer.model.NuSpecPackage;
import com.folib.nuget.utils.jaxb.JaxbFeedParser;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author LingengMa
 * @date 2025/05/06 09:04
 * @Description: 提取nuspec文件
 */


@Slf4j
public class NugetMetadataExtractor {
    @NonNull
    public NuSpecPackage extractNuspecFromStream(InputStream is) {
        try (ZipInputStream zipInputStream = new ZipInputStream(is)) {
            ZipEntry entry;
            while ((entry = zipInputStream.getNextEntry()) != null) {
                if (entry.getName().endsWith(".nuspec")) {
                    byte[] nuspecBytes = readEntryContent(zipInputStream);
                    try (ByteArrayInputStream nuspecStream = new ByteArrayInputStream(nuspecBytes)) {
                        return extractNuspecFromNuspecStream(nuspecStream);
                    }
                }
            }

            log.warn("Could not find .nuspec file entry, returning an invalid metadata");
            return new NuSpecPackage();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    @NonNull
    private NuSpecPackage extractNuspecFromNuspecStream(InputStream stream) {
        return JaxbFeedParser.extractNuspecFromNuspecStream(stream);
    }

    private byte[] readEntryContent(ZipInputStream zipIn) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        byte[] data = new byte[1024];
        int bytesRead;
        while ((bytesRead = zipIn.read(data)) != -1) {
            buffer.write(data, 0, bytesRead);
        }
        return buffer.toByteArray();
    }
}
