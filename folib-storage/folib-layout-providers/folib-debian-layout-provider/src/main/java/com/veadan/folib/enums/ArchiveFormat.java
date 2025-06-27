package com.veadan.folib.enums;

import com.github.luben.zstd.ZstdInputStream;
import lombok.Getter;
import org.apache.commons.compress.compressors.xz.XZCompressorInputStream;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

/**
 * @author veadan
 * @since 2024-09-03 10:53
 */
@Getter
public enum ArchiveFormat {

    GZ(".gz") {
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return new GZIPInputStream(controlArchiveStream);
        }
    },
    XZ(".xz") {
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return new XZCompressorInputStream(controlArchiveStream);
        }
    },
    ZX(".zst"){
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return new ZstdInputStream(controlArchiveStream);
        }

    },
    NONE("") {
        public InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException {
            return controlArchiveStream;
        }
    };

    private String extension;

     ArchiveFormat(String extension) {
        this.extension = extension;
    }

    public abstract InputStream unzipControl(ByteArrayInputStream controlArchiveStream) throws IOException;

}
