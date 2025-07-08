package com.folib.util.steam;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.zip.GZIPOutputStream;

/**
 * @author veadan
 * @since 2024-09-02 16:47
 */
@Slf4j
public class MetadataStreamGz  extends OutputToInputStream {

    private InputStream clearTextContentInput;

    public MetadataStreamGz(InputStream clearTextContentInput) {
        this.clearTextContentInput = clearTextContentInput;
    }

    protected void write(OutputStream sink) throws IOException {
        try (GZIPOutputStream gZippedOut = new GZIPOutputStream(sink)) {
            IOUtils.copyLarge(this.clearTextContentInput, gZippedOut);
            gZippedOut.finish();
        }catch (Exception e){
            log.error("Failed to compress Packages file content to Gz", e);
        }
    }
}
