package com.veadan.folib.commons.io.reloading;

import com.veadan.folib.commons.io.reloading.ReloadableInputStreamHandler;
import com.veadan.folib.commons.io.resource.ResourceCloser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * @author mtodorov
 */
public class FSReloadableInputStreamHandler
    implements ReloadableInputStreamHandler
{

    private static final Logger logger = LoggerFactory.getLogger(FSReloadableInputStreamHandler.class);

    private Path filePath;

    private InputStream inputStream;


    public FSReloadableInputStreamHandler(Path filePath)
    {
        this.filePath = filePath;
    }

    @Override
    public InputStream getInputStream()
            throws IOException
    {
        if (inputStream == null)
        {
            loadInputStream();
            return inputStream;
        }
        else
        {
            return inputStream;
        }
    }

    @Override
    public void reload()
            throws IOException
    {
        ResourceCloser.close(inputStream, logger);
        loadInputStream();
    }

    private void loadInputStream()
            throws IOException
    {
        inputStream = Files.newInputStream(filePath);
    }

}
