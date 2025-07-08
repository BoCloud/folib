package com.folib.interceptors;

import com.folib.storage.repository.Repository;


import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.web.servlet.HandlerInterceptor;

import java.io.IOException;

import static com.folib.web.Constants.*;


public class RepositoryRequestInterceptor implements HandlerInterceptor {
    @Override
    public boolean preHandle( HttpServletRequest request,
                              HttpServletResponse response,
                              Object handler) throws IOException {
        final String storageId = (String) request.getAttribute(STORAGE_NOT_FOUND_REQUEST_ATTRIBUTE);
        if (storageId != null) {
            response.sendError(HttpStatus.NOT_FOUND.value(), "The specified storage does not exist!");
            return false;
        }

        final String repositoryId = (String) request.getAttribute(REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE);
        if (repositoryId != null) {
            response.sendError(HttpStatus.NOT_FOUND.value(), "The specified repository does not exist!");
            return false;
        }

        final Repository repository = (Repository) request.getAttribute(REPOSITORY_REQUEST_ATTRIBUTE);
        if (repository != null && !repository.isInService()) {
            response.sendError(HttpStatus.SERVICE_UNAVAILABLE.value(), "Repository is not in service...");
            return false;
        }

        return true;
    }
}
