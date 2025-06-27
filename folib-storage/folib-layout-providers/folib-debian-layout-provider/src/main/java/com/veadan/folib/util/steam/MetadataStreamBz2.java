package com.veadan.folib.util.steam;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.CompressorOutputStream;
import org.apache.commons.compress.compressors.CompressorStreamFactory;
import org.apache.commons.io.IOUtils;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author veadan
 * @since 2024-09-02 17:10
 */
@Slf4j
public class MetadataStreamBz2 extends OutputToInputStream {

    private InputStream clearTextContentInput;

    public MetadataStreamBz2(InputStream clearTextContentInput) {
        this.clearTextContentInput = clearTextContentInput;
    }

    protected void write(OutputStream sink) {
        try (CompressorOutputStream bZippedOut = (new CompressorStreamFactory()).createCompressorOutputStream("bzip2", sink)) {
            IOUtils.copyLarge(this.clearTextContentInput, bZippedOut);
        } catch (Exception e) {
            log.error("Failed to compress Packages file content to Bz2", e);
        }

    }
}
