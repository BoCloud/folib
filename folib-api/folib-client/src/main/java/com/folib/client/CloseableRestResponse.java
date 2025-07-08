package com.folib.client;

import javax.ws.rs.core.Response;
import java.io.Closeable;
import java.io.IOException;

/**
 */
public class CloseableRestResponse
        implements Closeable
{

    private final Response response;

    public CloseableRestResponse(Response response)
    {
        this.response = response;
    }

    public Response getResponse()
    {
        return response;
    }

    @Override
    public void close()
            throws IOException
    {
        response.close();
    }
}
