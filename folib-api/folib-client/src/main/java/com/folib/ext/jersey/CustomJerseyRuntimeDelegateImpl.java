package com.folib.ext.jersey;

import javax.ws.rs.core.Application;

import org.glassfish.jersey.internal.AbstractRuntimeDelegate;
import org.glassfish.jersey.server.ContainerFactory;

/**
 * @author veadan
 * @see org.glassfish.jersey.server.internal.RuntimeDelegateImpl
 */
public class CustomJerseyRuntimeDelegateImpl
        extends AbstractRuntimeDelegate
{

    public CustomJerseyRuntimeDelegateImpl()
    {
        super((new CustomJerseyHeaderDelegateProviders()).getHeaderDelegateProviders());
    }

    @Override
    public <T> T createEndpoint(Application application,
                                Class<T> endpointType)
    {
        if (application == null)
        {
            throw new IllegalArgumentException("application is null.");
        }
        return ContainerFactory.createContainer(endpointType, application);
    }

}
