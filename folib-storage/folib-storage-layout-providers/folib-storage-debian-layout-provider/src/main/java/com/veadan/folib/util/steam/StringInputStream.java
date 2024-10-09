package com.veadan.folib.util.steam;

import javax.annotation.Nonnull;
import java.io.ByteArrayInputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * @author huayanjun
 * @since 2024-09-02 17:35
 */
public class StringInputStream extends ByteArrayInputStream {
    private final int length;

    public StringInputStream(@Nonnull String str) {
        this(str, StandardCharsets.UTF_8);
    }

    public StringInputStream(@Nonnull String str, @Nonnull Charset charset) {
        super(str.getBytes(charset));
        this.length = super.buf.length;
    }
    public int getLength() {
        return this.length;
    }
}
