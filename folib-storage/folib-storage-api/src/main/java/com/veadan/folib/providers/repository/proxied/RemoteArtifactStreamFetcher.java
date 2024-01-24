package com.veadan.folib.providers.repository.proxied;

import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.client.CloseableRestResponse;
import com.veadan.folib.client.RestArtifactResolver;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;

import javax.ws.rs.core.Response;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

public class RemoteArtifactStreamFetcher
{

    private RestArtifactResolver client;

    public RemoteArtifactStreamFetcher(RestArtifactResolver client)
    {
        super();
        this.client = client;
    }

    public InputStream getInputStream(long offset,
                                      RepositoryPath artifactPath)
        throws IOException
    {
        URI resource = getRestClientResourcePath(artifactPath);

        return new RemoteArtifactInputStream(resource, offset);
    }

    public String getHead(RepositoryPath repositoryPath)
        throws IOException
    {
        URI resource = getRestClientResourcePath(repositoryPath);
        try (final CloseableRestResponse closeableRestResponse = client.head(resource.toString()))
        {
            final Response response = closeableRestResponse.getResponse();

            if (response.getStatus() != 200 || response.getEntity() == null)
            {
                return null;
            }

            return response.getHeaderString("Accept-Ranges");
        }

    }

    private URI getRestClientResourcePath(final RepositoryPath artifactPath)
        throws IOException
    {
        return RepositoryFiles.resolveResource(artifactPath);
    }

    private CloseableRestResponse getConnection(URI resource,
                                                long offset)
        throws IOException
    {
        CloseableRestResponse connection = client.get(resource.toString(), offset);

        Response response = connection.getResponse();
        if (response.getStatus() == 404)
        {
            terminateConnection(connection);
            
            throw new ArtifactNotFoundException(resource);
        }
        if (response.getStatus() != 200 || response.getEntity() == null)
        {
            terminateConnection(connection);
            
            throw new IOException(String.format("Unreadable response for %s. Response status is %s",
                                                resource, response.getStatus()));
        }

        return connection;
    }

    private void terminateConnection(CloseableRestResponse connection)
    {
        try
        {
            connection.close();
        }
        catch (Exception e)
        {
            // ignore
        }
    }

    public class RemoteArtifactInputStream extends InputStream
    {

        private URI resource;
        private long offset;
        private CloseableRestResponse connection;
        private InputStream target;

        public RemoteArtifactInputStream(URI resource,
                                         long offset)
        {
            this.resource = resource;
            this.offset = offset;
        }

        public CloseableRestResponse getConnection()
            throws IOException
        {
            if (connection != null)
            {
                return connection;
            }
            return connection = RemoteArtifactStreamFetcher.this.getConnection(resource, offset);
        }

        private InputStream getTarget()
            throws IOException
        {
            if (target != null)
            {
                return target;
            }

            target = getConnection().getResponse().readEntity(InputStream.class);
            if (target == null)
            {
                throw new IOException(String.format("Unexpected null as InputStream response for %s.",
                                                    resource));
            }

            return target;
        }

        public int read()
            throws IOException
        {
            return getTarget().read();
        }

        public int read(byte[] b)
            throws IOException
        {
            return getTarget().read(b);
        }

        public int read(byte[] b,
                        int off,
                        int len)
            throws IOException
        {
            return getTarget().read(b, off, len);
        }

        public long skip(long n)
            throws IOException
        {
            return getTarget().skip(n);
        }

        public int available()
            throws IOException
        {
            return getTarget().available();
        }

        public void mark(int readlimit)
        {
            throw new UnsupportedOperationException();
        }

        public void reset()
            throws IOException
        {
            getTarget().reset();
        }

        public boolean markSupported()
        {
            return false;
        }

        @Override
        public void close()
            throws IOException
        {
            try
            {
                if (target != null)
                {
                    target.close();
                }
            } finally
            {
                closeConnection();
            }
        }

        private void closeConnection()
            throws IOException
        {
            try
            {
                if (connection != null)
                {
                    connection.close();
                }
            }
            catch (IOException e)
            {
                throw e;
            }
            catch (Exception e)
            {
                throw new IOException(e);
            }
        }

    }
    
}
