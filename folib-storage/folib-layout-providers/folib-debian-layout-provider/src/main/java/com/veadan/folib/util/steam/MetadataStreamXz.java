package com.veadan.folib.util.steam;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.CompressorOutputStream;
import org.apache.commons.compress.compressors.CompressorStreamFactory;
import org.apache.commons.io.IOUtils;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author huayanjun
 * @since 2024-09-02 17:11
 */
@Slf4j
public class MetadataStreamXz extends OutputToInputStream {
    private InputStream clearTextContentInput;

    public MetadataStreamXz(InputStream clearTextContentInput) {
        this.clearTextContentInput = clearTextContentInput;
    }

    protected void write(OutputStream sink) {
        try (CompressorOutputStream xzOut = (new CompressorStreamFactory()).createCompressorOutputStream("xz", sink)) {
            IOUtils.copyLarge(this.clearTextContentInput, xzOut);
        } catch (Exception e) {
            log.error("Failed to compress Packages file content to XZ", e);
        }
    }
}
