/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.controllers;

import com.folib.controllers.support.ErrorResponseEntityBody;
import com.folib.controllers.support.ResponseEntityBody;
import com.folib.data.criteria.QueryParserException;
import com.folib.exception.Http202PropogateException;
import com.folib.exception.RepositoryNotFoundException;
import com.folib.exception.ServiceUnavailableException;
import com.folib.exception.StorageNotFoundException;
import com.folib.scanner.common.exception.BusinessException;
import com.folib.validation.RequestBodyValidationError;
import com.folib.validation.RequestBodyValidationException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import jakarta.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.lang.Nullable;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.validation.FieldError;
import org.springframework.web.HttpMediaTypeNotAcceptableException;
import org.springframework.web.accept.ContentNegotiationManager;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
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
    @ResponseStatus(HttpStatus.FORBIDDEN)
    protected ResponseEntity<?> handleAccessDeniedException(AccessDeniedException ex,
                                                            WebRequest request,
                                                            HttpServletRequest httpRequest,
                                                            HttpServletResponse httpResponse)
    {
       // throw ex;
        return ResponseEntity.status(HttpStatus.FORBIDDEN.value()).body(ex.getMessage());
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

    @ExceptionHandler(BusinessException.class)
    protected ResponseEntity<?> handleBusinessException(BusinessException ex,
                                                   WebRequest request)
    {
        ResponseEntityBody body = new ResponseEntityBody(ex.getMessage());
        HttpHeaders headers = new HttpHeaders();
        return handleExceptionInternal(ex, body, headers, HttpStatus.INTERNAL_SERVER_ERROR, request);
    }

    @ExceptionHandler(Exception.class)
    protected ResponseEntity<?> handleUnknownError(Exception ex,
                                                   WebRequest request)
    {
        logger.error("Request [{}] failed.", request, ex);
        
        return provideDefaultErrorResponse(ex, request, HttpStatus.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler(AuthenticationException.class)
    public ResponseEntity<?> handleAuthenticationException(AuthenticationException ex) {
        return ResponseEntity.status(401).body(Map.of("error", ex.getMessage()));
    }

    @Override
    protected ResponseEntity<Object> handleExceptionInternal(Exception ex,
                                                             @Nullable Object body,
                                                             HttpHeaders headers,
                                                             HttpStatusCode statusCode,
                                                             WebRequest request)
    {
        // 1. 创建可修改的 headers 副本（先复制，再操作）
        HttpHeaders mutableHeaders = new HttpHeaders();
        if (headers != null) {
            mutableHeaders.putAll(headers);  // 避免原始 headers 被污染
        }

        // 2. 在副本上设置 Content-Type
        MediaType contentType = requestedContent(request);
        mutableHeaders.set(HttpHeaders.CONTENT_TYPE, contentType.toString());

        // 3. 处理 body 内容
        if (contentType.equals(MediaType.TEXT_PLAIN)) {
            body = ex.getMessage();
        } else if (body == null) {
            body = new ErrorResponseEntityBody(ex.getMessage());
        }

        // 4. 传递 mutableHeaders 给父类方法
        return super.handleExceptionInternal(ex, body, mutableHeaders, statusCode, request);
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
