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
package com.folib.web;

import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.exception.RepositoryNotFoundException;
import com.folib.exception.ServiceUnavailableException;
import com.folib.exception.StorageNotFoundException;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.MethodParameter;
import org.springframework.web.bind.MissingPathVariableException;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import org.springframework.web.servlet.HandlerMapping;

import javax.inject.Inject;
import java.util.Map;
import java.util.Objects;

import static com.folib.web.Constants.REPOSITORY_REQUEST_ATTRIBUTE;

/**
 * @author Veadan
 */
@Slf4j
public class RepositoryMethodArgumentResolver
        implements HandlerMethodArgumentResolver {

    public static final String NOT_FOUND_STORAGE_MESSAGE = "Could not find requested storage %s.";
    public static final String NOT_FOUND_REPOSITORY_MESSAGE = "Could not find requested repository %s:%s.";
    public static final String NOT_IN_SERVICE_REPOSITORY_MESSAGE = "Requested repository %s:%s is out of service.";

    @Inject
    protected ConfigurationManager configurationManager;

    @Override
    public boolean supportsParameter(final MethodParameter parameter) {

        // Check parameter annotation type
        if (!parameter.hasParameterAnnotation(RepoMapping.class)) {
            return false;
        }
        // Check parameter type.
        return parameter.getParameterType().equals(Repository.class);
    }

    @Override
    public Object resolveArgument(final MethodParameter parameter,
                                  final ModelAndViewContainer modelAndViewContainer,
                                  final NativeWebRequest nativeWebRequest,
                                  final WebDataBinderFactory webDataBinderFactory)
            throws MissingPathVariableException {
        final RepoMapping repositoryMapping = parameter.getParameterAnnotation(RepoMapping.class);
        final String storageVariableName = repositoryMapping.storageVariableName();
        final String storageId = getRequiredPathVariable(parameter, nativeWebRequest, storageVariableName);

        final String repositoryVariableName = repositoryMapping.repositoryVariableName();
        final String repositoryId = getRequiredPathVariable(parameter, nativeWebRequest, repositoryVariableName);

        Repository repository = (Repository) nativeWebRequest.getAttribute(REPOSITORY_REQUEST_ATTRIBUTE,
                RequestAttributes.SCOPE_REQUEST);

        if (repository != null && Objects.equals(repository.getId(), repositoryId) &&
                Objects.equals(repository.getStorage().getId(), storageId)) {
            return repository;
        }

        final Storage storage = getStorage(storageId);
        if (storage == null) {
            final String message = String.format(NOT_FOUND_STORAGE_MESSAGE, storageId);
            throw new StorageNotFoundException(message);
        }

        repository = getRepository(storageId, repositoryId);
        if (repository == null) {
            final String message = String.format(NOT_FOUND_REPOSITORY_MESSAGE, storageId, repositoryId);
            throw new RepositoryNotFoundException(message);
        }
        // This annotation is used in a lot of controllers - some of which are related to the configuration management.
        // It is necessary to allow requests to pass when the repository status is `out of service` (i.e. `/api/configuration/**`),
        // but still return `ServiceUnavailableException` when people are accessing `/storages/**`.
        if (!repository.isInService() && !repositoryMapping.allowOutOfServiceRepository()) {
            final String message = String.format(NOT_IN_SERVICE_REPOSITORY_MESSAGE, storageId, repositoryId);
            throw new ServiceUnavailableException(message);
        }
        return repository;
    }

    private String getRequiredPathVariable(final MethodParameter parameter,
                                           final NativeWebRequest nativeWebRequest,
                                           final String variableName)
            throws MissingPathVariableException {
        // Check @PathVariable parameter.
        @SuppressWarnings("unchecked") final Map<String, String> uriTemplateVars = (Map<String, String>) nativeWebRequest.getAttribute(
                HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE, RequestAttributes.SCOPE_REQUEST);
        if (MapUtils.isNotEmpty(uriTemplateVars)) {
            final String pathVariable = uriTemplateVars.get(variableName);
            if (StringUtils.isNotEmpty(pathVariable)) {
                return pathVariable;
            }
        }

        // Check @RequestParam parameter.
        final String requestParam = nativeWebRequest.getParameter(variableName);
        if (StringUtils.isNotEmpty(requestParam)) {
            return requestParam;
        }

        throw new MissingPathVariableException(variableName, parameter);
    }

    private Storage getStorage(final String storageId) {
        final Configuration configuration = configurationManager.getConfiguration();
        if (configuration == null) {
            return null;
        }
        return configuration.getStorage(storageId);
    }

    private Repository getRepository(final String storageId,
                                     final String repositoryId) {
        final Storage storage = getStorage(storageId);
        if (storage == null) {
            return null;
        }
        return storage.getRepository(repositoryId);
    }

}
