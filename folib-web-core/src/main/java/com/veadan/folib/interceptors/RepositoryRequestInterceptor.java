package com.veadan.folib.interceptors;

import com.veadan.folib.storage.repository.Repository;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

import org.springframework.http.HttpStatus;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;
import static com.veadan.folib.web.Constants.*;
import static org.springframework.http.HttpStatus.NOT_FOUND;

/**
 * @author Przemyslaw Fusik
 */
public class RepositoryRequestInterceptor
        extends HandlerInterceptorAdapter
{

    @Override
    public boolean preHandle(final HttpServletRequest request,
                             final HttpServletResponse response,
                             final Object handler)
            throws IOException
    {
        final String storageId = (String) request.getAttribute(STORAGE_NOT_FOUND_REQUEST_ATTRIBUTE);
        if (storageId != null)
        {
            response.sendError(NOT_FOUND.value(), "The specified storage does not exist!");
            return false;
        }
        final String repositoryId = (String) request.getAttribute(REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE);
        if (repositoryId != null)
        {
            response.sendError(NOT_FOUND.value(), "The specified repository does not exist!");
            return false;
        }

        final Repository repository = (Repository) request.getAttribute(REPOSITORY_REQUEST_ATTRIBUTE);
        if (repository != null && !repository.isInService())
        {
            response.sendError(HttpStatus.SERVICE_UNAVAILABLE.value(), "Repository is not in service...");
            return false;
        }

        return true;
    }
}
