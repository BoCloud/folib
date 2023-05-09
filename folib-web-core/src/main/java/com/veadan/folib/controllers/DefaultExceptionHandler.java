package com.veadan.folib.controllers;

import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.controllers.support.ResponseEntityBody;
import com.veadan.folib.data.criteria.QueryParserException;
import com.veadan.folib.exception.Http202PropogateException;
import com.veadan.folib.exception.RepositoryNotFoundException;
import com.veadan.folib.exception.ServiceUnavailableException;
import com.veadan.folib.exception.StorageNotFoundException;
import com.veadan.folib.validation.RequestBodyValidationError;
import com.veadan.folib.validation.RequestBodyValidationException;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.Nullable;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpMediaTypeNotAcceptableException;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class DefaultExceptionHandler extends ResponseEntityExceptionHandler
{
    private final Logger logger = LoggerFactory.getLogger(DefaultExceptionHandler.class);

    @Inject
    private ContentNegotiationManager contentNegotiationManager;

    @ExceptionHandler({ QueryParserException.class })
    protected ResponseEntity<?> handleRequestParseException(Exception ex,
                                                            WebRequest request)
    {
        return provideDefaultErrorResponse(ex, request, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(AccessDeniedException.class)
    protected ResponseEntity<?> handleAccessDeniedException(AccessDeniedException ex,
                                                            WebRequest request,
                                                            HttpServletRequest httpRequest,
                                                            HttpServletResponse httpResponse)
    {
        throw ex;
    }

    @ExceptionHandler(RequestBodyValidationException.class)
    protected ResponseEntity<?> handleRequestBodyValidationException(final RequestBodyValidationException ex,
                                                                     final WebRequest request)
    {
        return provideValidationErrorResponse(ex, request);
    }

    @ExceptionHandler(StorageNotFoundException.class)
    protected ResponseEntity<?> handleStorageNotFoundException(final StorageNotFoundException ex,
                                                               final WebRequest request)
    {
        ResponseEntityBody body = new ResponseEntityBody(ex.getMessage());
        HttpHeaders headers = new HttpHeaders();
        return handleExceptionInternal(ex, body, headers, HttpStatus.NOT_FOUND, request);
    }

    @ExceptionHandler(RepositoryNotFoundException.class)
    protected ResponseEntity<?> handleRepositoryNotFoundException(final RepositoryNotFoundException ex,
                                                                  final WebRequest request)
    {
        ResponseEntityBody body = new ResponseEntityBody(ex.getMessage());
        HttpHeaders headers = new HttpHeaders();
        return handleExceptionInternal(ex, body, headers, HttpStatus.NOT_FOUND, request);
    }

    @ExceptionHandler(ServiceUnavailableException.class)
    protected ResponseEntity<?> handleServiceUnavailableException(final ServiceUnavailableException ex,
                                                                  final WebRequest request)
    {
        ResponseEntityBody body = new ResponseEntityBody(ex.getMessage());
        HttpHeaders headers = new HttpHeaders();
        return handleExceptionInternal(ex, body, headers, HttpStatus.SERVICE_UNAVAILABLE, request);
    }
    
    @ExceptionHandler(Http202PropogateException.class)
    protected void handleHttp202PropogateException(Exception ex, HttpServletResponse httpResponse)
    {
        logger.info(ex.getMessage());
        httpResponse.setStatus(202);
    }

    @ExceptionHandler(Exception.class)
    protected ResponseEntity<?> handleUnknownError(Exception ex,
                                                   WebRequest request)
    {
        logger.error("Request [{}] failed.", request, ex);
        
        return provideDefaultErrorResponse(ex, request, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @Override
    protected ResponseEntity<Object> handleExceptionInternal(Exception ex,
                                                             @Nullable Object body,
                                                             HttpHeaders headers,
                                                             HttpStatus status,
                                                             WebRequest request)
    {
        MediaType contentType = requestedContent(request);
        headers.set(HttpHeaders.CONTENT_TYPE, contentType.toString());

        if (contentType.equals(MediaType.TEXT_PLAIN))
        {
            body = ex.getMessage();
        }
        else if (body == null)
        {
            body = new ErrorResponseEntityBody(ex.getMessage());
        }

        return super.handleExceptionInternal(ex, body, headers, status, request);
    }

    private ResponseEntity<?> provideValidationErrorResponse(final RequestBodyValidationException ex,
                                                             final WebRequest request)
    {
        final RequestBodyValidationError validationError = new RequestBodyValidationError(ex.getMessage());

        final List<FieldError> fieldErrors = ex.getErrors().getFieldErrors();
        for (final FieldError fieldError : fieldErrors)
        {
            validationError.add(fieldError.getField(), fieldError.getDefaultMessage());
        }

        final HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        return handleExceptionInternal(ex, validationError, headers, HttpStatus.BAD_REQUEST, request);
    }

    private ResponseEntity<Object> provideDefaultErrorResponse(Exception ex,
                                                               WebRequest request,
                                                               HttpStatus status)
    {
        return handleExceptionInternal(ex, null, new HttpHeaders(), status, request);
    }

    private MediaType requestedContent(WebRequest request)
    {
        List<MediaType> mediaTypes = new ArrayList<>();
        try
        {
            mediaTypes.addAll(contentNegotiationManager.resolveMediaTypes((NativeWebRequest) request));
        }
        catch (HttpMediaTypeNotAcceptableException e1)
        {
            logger.error("Requested invalid content-type [{}]", request.getHeader(HttpHeaders.ACCEPT), e1);
            mediaTypes.add(MediaType.APPLICATION_JSON);
        }

        MediaType result = mediaTypes.stream()
                                     .reduce(null, this::reduceByPriority);

        return Optional.ofNullable(result).orElse(MediaType.APPLICATION_JSON);
    }

    private MediaType reduceByPriority(MediaType m1,
                                       MediaType m2)
    {
        if (MediaType.APPLICATION_JSON.equals(m1) || MediaType.APPLICATION_JSON.equals(m2))
        {
            return MediaType.APPLICATION_JSON;
        }
        if (MediaType.APPLICATION_XML.equals(m1) || MediaType.APPLICATION_XML.equals(m2))
        {
            return MediaType.APPLICATION_XML;
        }
        if (MediaType.TEXT_PLAIN.equals(m1) || MediaType.TEXT_PLAIN.equals(m2))
        {
            return MediaType.TEXT_PLAIN;
        }
        return null;
    }

}
