package com.veadan.folib.controllers;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.controllers.support.ErrorResponseEntityBody;
import com.veadan.folib.controllers.support.ListEntityBody;
import com.veadan.folib.controllers.support.ResponseEntityBody;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.exception.ExceptionHandlingOutputStream;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.collections4.IteratorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

/**
 * Provides common subroutines that will be useful for any backend controllers.
 *
 * @author Alex Oreshkevich
 * @author Veadan
 * @author veadan
 */
public abstract class BaseController
{

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    protected ObjectMapper objectMapper;

    @Inject
    protected ConfigurationManagementService configurationManagementService;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    protected Configuration getConfiguration()
    {
        return configurationManagementService.getConfiguration();
    }

    protected Storage getStorage(String storageId)
    {
        return configurationManager.getConfiguration().getStorage(storageId);
    }

    protected Repository getRepository(String storageId,
                                       String repositoryId)
    {
        return getStorage(storageId).getRepository(repositoryId);
    }

    protected MutableConfiguration getMutableConfigurationClone()
    {
        return configurationManagementService.getMutableConfigurationClone();
    }

    /**
     * You will rarely need to use this method directly. In most cases you should consider using the methods below
     * to guarantee consistency in the returned responses to the client request.
     *
     * @param message      Message to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return Object
     */
    protected Object getResponseEntityBody(String message,
                                           String acceptHeader)
    {
        if (acceptHeader != null && !acceptHeader.isEmpty())
        {
            acceptHeader = acceptHeader.toLowerCase();
            if ((acceptHeader.contains(MediaType.TEXT_PLAIN_VALUE.toLowerCase()) ||
                 acceptHeader.contains(MediaType.TEXT_HTML_VALUE.toLowerCase())))
            {
                return message;
            }
        }

        return new ResponseEntityBody(message);
    }

    /**
     * @param fieldName JSON field name
     * @param list      the list
     * @return
     */
    protected ResponseEntity getJSONListResponseEntityBody(String fieldName,
                                                           List<?> list)
    {
        return ResponseEntity.ok(new ListEntityBody(fieldName, list));
    }

    /**
     * @param fieldName JSON field name
     * @param iterable  the iterable
     * @return ResponseEntity
     */
    protected ResponseEntity getJSONListResponseEntityBody(String fieldName,
                                                           final Iterable<?> iterable)
    {
        List<?> list = IteratorUtils.toList(iterable.iterator());
        return getJSONListResponseEntityBody(fieldName, list);
    }

    /**
     * Used for operations which have been successfully performed.
     *
     * @param message       Success to be returned to the client.
     * @param headers       response headers
     * @param acceptHeader  The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getSuccessfulResponseEntity(String message, HttpHeaders headers, String acceptHeader)
    {
        return ResponseEntity.ok().headers(headers).body(getResponseEntityBody(message, acceptHeader));
    }

    /**
     * Used for operations which have been successfully performed.
     *
     * @param message      Success to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getSuccessfulResponseEntity(String message,
                                                         String acceptHeader)
    {
        return getSuccessfulResponseEntity(message, null, acceptHeader);
    }

    /**
     * Used for operations which have failed.
     *
     * @param status       Status code to be returned (i.e. 400 Bad Request)
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getFailedResponseEntity(HttpStatus status,
                                                     String message,
                                                     String acceptHeader)
    {
        return ResponseEntity.status(status)
                             .body(getResponseEntityBody(message, acceptHeader));
    }

    /**
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getBadRequestResponseEntity(String message,
                                                         String acceptHeader)
    {
        return getFailedResponseEntity(HttpStatus.BAD_REQUEST, message, acceptHeader);
    }

    /**
     * Used in cases where resource could not be found.
     *
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getNotFoundResponseEntity(String message,
                                                       String acceptHeader)
    {
        return ResponseEntity.status(HttpStatus.NOT_FOUND)
                             .body(getResponseEntityBody(message, acceptHeader));

    }

    /**
     * Used in cases where resource is not available not be found.
     *
     * @param message      Error to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getServiceUnavailableResponseEntity(String message,
                                                                 String acceptHeader)
    {
        return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE)
                             .body(getResponseEntityBody(message, acceptHeader));
    }

    /**
     * @param message      Error message to be returned to the client.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getRuntimeExceptionResponseEntity(String message,
                                                               String acceptHeader)
    {
        return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR,
                                          new RuntimeException(message),
                                          acceptHeader);
    }

    /**
     * @param httpStatus   HttpStatus to be returned.
     * @param cause        Exception.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getExceptionResponseEntity(HttpStatus httpStatus,
                                                        Throwable cause,
                                                        String acceptHeader)
    {
        return getExceptionResponseEntity(httpStatus, cause.getMessage(), cause, acceptHeader);
    }

    /**
     * @param httpStatus   HttpStatus to be returned.
     * @param message      Error message to display in the logs and returned to the client.
     * @param cause        Exception.
     * @param acceptHeader The Accept header, so that we can return the proper json/plain text response.
     * @return ResponseEntity
     */
    protected ResponseEntity getExceptionResponseEntity(HttpStatus httpStatus,
                                                        String message,
                                                        Throwable cause,
                                                        String acceptHeader)
    {
        logger.error(message, cause);

        Object responseEntityBody = getResponseEntityBody(message, acceptHeader);
        return ResponseEntity.status(httpStatus)
                             .body(responseEntityBody);
    }

    /**
     * Used to stream files to the client.
     *
     * @param is       InputStream
     * @param filename String
     * @return ResponseEntity
     * @throws IllegalStateException
     */
    protected ResponseEntity<InputStreamResource> getStreamToResponseEntity(InputStream is,
                                                                            String filename)
            throws IllegalStateException
    {
        InputStreamResource inputStreamResource = new InputStreamResource(is);

        HttpHeaders httpHeaders = new HttpHeaders();
        httpHeaders.set("Content-Disposition", "attachment; filename=" + filename);

        return new ResponseEntity<>(inputStreamResource, httpHeaders, HttpStatus.OK);
    }

    // TODO: The methods below are obsolete and should be gradually removed from usage. We'll maybe only keep copyToResponse.
    protected ResponseEntity toResponseEntityError(String message,
                                                   HttpStatus httpStatus)
    {
        return ResponseEntity.status(httpStatus)
                             .body(new ErrorResponseEntityBody(message));
    }

    protected ResponseEntity toResponseEntityError(String message)
    {
        return toResponseEntityError(message, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    public static void copyToResponse(InputStream is,
                                      HttpServletResponse response)
            throws IOException
    {
        try (OutputStream os = new ExceptionHandlingOutputStream(response.getOutputStream()))
        {
            long totalBytes = 0L;

            int readLength;
            byte[] bytes = new byte[4096];
            while ((readLength = is.read(bytes)) != -1)
            {
                // Write the artifact
                os.write(bytes, 0, readLength);
                os.flush();

                totalBytes += readLength;
            }

            response.setHeader(HttpHeaders.CONTENT_LENGTH, Long.toString(totalBytes));
            response.flushBuffer();
        }
    }
}
