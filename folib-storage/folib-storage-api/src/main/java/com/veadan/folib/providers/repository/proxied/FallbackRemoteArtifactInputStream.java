package com.veadan.folib.providers.repository.proxied;

import com.veadan.folib.artifact.ArtifactNotFoundException;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.function.Function;

/**
 * @author pengYongQiang
 * @date 1/12/2024 20:36
 * <p>
 * Non Thread safe
 */
public class FallbackRemoteArtifactInputStream extends FilterInputStream {

    private final Function<Exception, InputStream> fallbackInputStreamSupplier;
    private boolean usingFallback = false;

    protected FallbackRemoteArtifactInputStream(InputStream primaryInputStream
            , Function<Exception, InputStream> fallbackInputStreamSupplier) {
        super(primaryInputStream);
        this.fallbackInputStreamSupplier = fallbackInputStreamSupplier;
    }
    private void switchToFallback(Exception e) throws ArtifactNotFoundException {
        if (usingFallback) {
            if (e instanceof ArtifactNotFoundException){
                throw (ArtifactNotFoundException)e;
            }
            throw new RuntimeException("Fallback Exception", e);
        }
        usingFallback = true;
        try {
            super.close();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
        InputStream apply = fallbackInputStreamSupplier.apply(e);
        if (apply == null) {
            throw new RuntimeException(e);
        }
        super.in = apply;

    }

    @Override
    public int read() throws IOException {
        try {
            return super.read();
        } catch (Exception e) {
            switchToFallback(e);
            return read();
        }
    }

    @Override
    public int read( byte[] b) throws IOException {
        try {
            return super.read(b);
        } catch (Exception e) {
            switchToFallback(e);
            return read(b);
        }

    }

    @Override
    public int read( byte[] b, int off, int len) throws IOException {
        try {
            return super.read(b, off, len);
        } catch (Exception e) {
            switchToFallback(e);
            return read(b, off, len);
        }
    }

    @Override
    public long skip(long n) throws IOException {
        try {
            return super.skip(n);
        } catch (Exception e) {
            switchToFallback(e);
            return skip(n);
        }
    }

    @Override
    public void close() throws IOException {
        super.close();
    }

    @Override
    public void mark(int readlimit) {
        try {
            super.mark(readlimit);
        } catch (Exception e) {
            try {
                switchToFallback(e);
            } catch (ArtifactNotFoundException ex) {
                throw new RuntimeException(ex);
            }
            mark(readlimit);
        }
    }

    @Override
    public void reset() throws IOException {
        try {
            super.reset();
        } catch (Exception e) {
            switchToFallback(e);
            reset();
        }
    }

    @Override
    public boolean markSupported() {
        try {
            return super.markSupported();
        } catch (Exception e) {
            try {
                switchToFallback(e);
            } catch (ArtifactNotFoundException ex) {
                throw new RuntimeException(ex);
            }
            return markSupported();
        }
    }

    @Override
    public int available() throws IOException {
        try {
            return super.available();
        } catch (Exception e) {
            switchToFallback(e);
            return available();
        }
    }
}
