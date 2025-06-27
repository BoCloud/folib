package com.veadan.folib.util.steam;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.CompressorOutputStream;
import org.apache.commons.compress.compressors.CompressorStreamFactory;
import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author veadan
 * @since 2024-09-02 17:11
 */
@Slf4j
public class MetadataStreamLzma extends OutputToInputStream {
    private InputStream clearTextContentInput;

    public MetadataStreamLzma(InputStream clearTextContentInput) {
        this.clearTextContentInput = clearTextContentInput;
    }

    protected void write(OutputStream sink) throws IOException {
        try (CompressorOutputStream lzmaOut = (new CompressorStreamFactory()).createCompressorOutputStream("lzma", sink)) {
            IOUtils.copyLarge(this.clearTextContentInput, lzmaOut);
        } catch (Exception e) {
            log.info("Failed to compress Packages file content to LZMA", e);
        }
    }
}
