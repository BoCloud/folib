package com.folib.web.system;

import com.folib.controllers.support.ErrorResponseEntityBody;
import com.folib.providers.io.RepositoryRelativePathConstructionException;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

/**
 * @author veadan
 */
@ControllerAdvice
public class RepositoryPathConstructionExceptionHandler
        extends ResponseEntityExceptionHandler
{

    @ExceptionHandler(RepositoryRelativePathConstructionException.class)
    protected ResponseEntity<?> handleRepositoryRelativePathConstructionException(final RepositoryRelativePathConstructionException ex,
                                                                                  final WebRequest request)
    {
        final HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        return handleExceptionInternal(ex,
                                       new ErrorResponseEntityBody("Only valid relative paths are allowed"),
                                       headers,
                                       HttpStatus.BAD_REQUEST, request);
    }
}
