/**
 * 
 */
package com.veadan.folib.exception;

import java.io.IOException;
import java.io.OutputStream;
import org.eclipse.jetty.io.EofException;
import org.apache.commons.io.output.ProxyOutputStream;

/**
 * @author Bogdan Sukonnov
 *
 */
public class ExceptionHandlingOutputStream extends ProxyOutputStream
{
    
    /**
     * @param proxy
     */
    public ExceptionHandlingOutputStream(OutputStream proxy)
    {
        super(proxy);
    }

    @Override
    protected void handleIOException(IOException e)
        throws IOException
    {
        if (e.getClass().equals(EofException.class))
        {
            throw new Http202PropogateException("Socket has been closed. Possibly, user cancelled download.", e);            
        }
        throw e;
    }

}
