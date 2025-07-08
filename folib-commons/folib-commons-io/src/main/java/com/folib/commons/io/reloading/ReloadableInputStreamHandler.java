package com.folib.commons.io.reloading;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author veadan
 */
public interface ReloadableInputStreamHandler extends Reloading
{

    InputStream getInputStream()
            throws IOException;

}
