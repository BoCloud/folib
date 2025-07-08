package com.folib.utils;

import java.io.Closeable;
import lombok.Generated;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class StreamUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(StreamUtils.class);

    @Generated
    private StreamUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    public static void close(Closeable closeable) {
        try {
            if (closeable != null) {
                closeable.close();
            }
        } catch (Exception e) {
            log.debug("", e);
        }
    }
}

