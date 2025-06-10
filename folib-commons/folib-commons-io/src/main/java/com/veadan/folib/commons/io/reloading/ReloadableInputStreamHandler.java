package com.veadan.folib.commons.io.reloading;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author mtodorov
 */
public interface ReloadableInputStreamHandler extends Reloading
{

    InputStream getInputStream()
            throws IOException;

}
