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
package com.folib.storage.validation.version;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.storage.validation.artifact.version.VersionValidationException;
import com.folib.storage.repository.Repository;

import javax.annotation.PostConstruct;
import java.util.function.BiPredicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Regex pattern used here is following Canonical Version format specified in PEP 440 (https://www.python.org/dev/peps/pep-0440/)
 * This Validator ensures strict compliance to the Canonical (normalized) form of the version.
 * Full Canonical pattern is ^((\\d+!)?(\\d+(\\.\\d+)*)((a|b|c|rc)\\d+)?(\\.post\\d+)?\\+[a-zA-Z0-9](\\.*[a-zA-Z0-9])*)?)$
 *
 * @author sainalshah
 */
@Component
public class PypiCanonicalVersionValidator
        implements PypiVersionValidator
{

    private static final String EPOCH_FORMAT = "(\\d+!)";
    private static final String FINAL_RELEASE_FORMAT = "(\\d+(\\.\\d+)*)";
    private static final String PRE_RELEASE_FORMAT = "((a|b|c|rc)\\d+)";
    private static final String POST_RELEASE_FORMAT = "(\\.post\\d+)";
    private static final String DEVELOPMENTAL_RELEASE_FORMAT = "(\\.dev\\d+)";

    //must start and end with ASCII character, can have periods in between
    private static final String LOCAL_VERSION_IDENTIFIER_FORMAT = "(\\+[a-zA-Z0-9](\\.*[a-zA-Z0-9])*)";
    private static final String CANONICAL_VERSION_FORMAT = getCanonicalVersionFormat();


    private static final Pattern PRE_RELEASE_PATTERN = Pattern.compile(
            PypiCanonicalVersionValidator.PRE_RELEASE_FORMAT);
    private static final Pattern POST_RELEASE_PATTERN = Pattern.compile(
            PypiCanonicalVersionValidator.POST_RELEASE_FORMAT);
    private static final Pattern DEVELOPMENTAL_RELEASE_PATTERN = Pattern.compile(
            PypiCanonicalVersionValidator.DEVELOPMENTAL_RELEASE_FORMAT);
    private static final Pattern LOCAL_VERSION_IDENTIFIER_PATTERN = Pattern.compile(
            PypiCanonicalVersionValidator.LOCAL_VERSION_IDENTIFIER_FORMAT);
    private static final Pattern CANONICAL_PATTERN = Pattern.compile(
            PypiCanonicalVersionValidator.CANONICAL_VERSION_FORMAT);

    private final ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;

    private BiPredicate<Pattern, String> versionMatches = (pattern, version) -> pattern.matcher(version).matches();

    private static final Logger logger = LoggerFactory.getLogger(PypiCanonicalVersionValidator.class);

    public static final String ALIAS = "pypi-canonical-version-validator";

    public static final String DESCRIPTION = "PyPi canonical version validator";

    public PypiCanonicalVersionValidator(ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry)
    {
        this.artifactCoordinatesValidatorRegistry = artifactCoordinatesValidatorRegistry;
    }

    @Override
    public String getDescription()
    {
        return DESCRIPTION;
    }

    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException
    {
        final Matcher matcher = PypiCanonicalVersionValidator.CANONICAL_PATTERN.matcher(coordinates.getVersion());
        if (!matcher.matches())
        {
            throw new VersionValidationException(String.format("Artifact version [%s] should follow the Canonical " +
                                                               "Versioning specification (https://www.python.org/dev/peps/pep-0440/).",
                                                               coordinates.getVersion()));
        }
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @PostConstruct
    @Override
    public void register()
    {
        artifactCoordinatesValidatorRegistry.addProvider(ALIAS, this);

        logger.info("Registered artifact coordinates validator '{}' with alias '{}'.", getClass().getCanonicalName(),
                    ALIAS);
    }

    public boolean isPreRelease(String version)
    {
        return versionMatches.test(PypiCanonicalVersionValidator.PRE_RELEASE_PATTERN, version);
    }

    public boolean isPostRelease(String version)
    {
        return versionMatches.test(PypiCanonicalVersionValidator.POST_RELEASE_PATTERN, version);
    }

    public boolean isDevelopmentalRelease(String version)
    {
        return versionMatches.test(PypiCanonicalVersionValidator.DEVELOPMENTAL_RELEASE_PATTERN, version);
    }

    public boolean isLocalVersionIdentifierRelease(String version)
    {
        return versionMatches.test(PypiCanonicalVersionValidator.LOCAL_VERSION_IDENTIFIER_PATTERN, version);
    }

    public boolean isFinalRelease(String version)
    {
        return !isPreRelease(version) && !isPostRelease(version) && !isDevelopmentalRelease(version) &&
               !isLocalVersionIdentifierRelease(version);
    }

    private static String getCanonicalVersionFormat()
    {
        return "^(" + EPOCH_FORMAT + "?" + FINAL_RELEASE_FORMAT + PRE_RELEASE_FORMAT + "?" + POST_RELEASE_FORMAT + "?" +
               DEVELOPMENTAL_RELEASE_FORMAT + "?" + LOCAL_VERSION_IDENTIFIER_FORMAT + "?)$";
    }
}
