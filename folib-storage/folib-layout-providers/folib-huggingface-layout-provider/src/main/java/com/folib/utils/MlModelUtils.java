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
package com.folib.utils;


import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.paramnames.ParameterNamesModule;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URI;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.ws.rs.core.Response;

import com.folib.common.SafeUriDeserializer;
import com.folib.constant.MlModelConstants;
import com.folib.model.CardData;
import com.folib.model.RevisionData;
import com.folib.model.request.MlModelRequestContext;
import lombok.Generated;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class MlModelUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(MlModelUtils.class);

    private static final String LFS_UPLOAD_ENDPOINT_TEMPLATE = "api/%s/%s/api/complete_multipart";

    private static final String MODELS = "models";

    @Generated
    private MlModelUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    private static final Pattern MD_MODEL_NAME_PATTERN = Pattern.compile("^[A-Za-z0-9\\-._]+$");

    private static final Pattern ORGANIZATION_NAME_PATTERN = Pattern.compile("\\b(?!\\d+$)([a-zA-Z0-9]|-(?!-)){2,42}\\b");

    private static final Pattern REVISION_NAME_PATTERN = Pattern.compile("^[a-zA-Z0-9\\-\\.]+$");

    private static final Pattern SHA_ONE_PATTERN = Pattern.compile("^[0-9a-f]{40}$");

    private static final String INTEGRATION_BRANCH = "main";

    public static ObjectMapper createObjectMapper() {
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true);
        ParameterNamesModule module = new ParameterNamesModule();
        module.addDeserializer(URI.class,  new SafeUriDeserializer());
        mapper.registerModule(module);
        return mapper;
    }

    public static boolean isReleaseRevision(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        return !"main".equals(context.getRevision());
    }

    public static boolean isSha1Value(String revision) {
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        return isValidStringWithPattern(revision, SHA_ONE_PATTERN);
    }

    public static String getModelId(String organization, String modelName) {
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (StringUtils.isBlank(organization)) {
            return modelName;
        }
        return String.join("/", organization, modelName);
    }

    public static String getFilePath(String organization, String model, String revision, String timestamp, String fileName) {
        if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        }
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        if (timestamp == null) {
            throw new NullPointerException("timestamp is marked non-null but is null");
        }
        if (fileName == null) {
            throw new NullPointerException("fileName is marked non-null but is null");
        }
        return String.join("/", getModelPath(organization, model), revision, timestamp, fileName);
    }

    public static String getLatestLeadFilePath(String organization, String model, String revision) {
        if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        }
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        if (StringUtils.isBlank(organization)) {
            return String.join("/", "models", model, revision, ".latest_huggingface_model_info.json");
        }
        return String.join("/", getModelPath(organization, model), revision, ".latest_huggingface_model_info.json");
    }

    public static String getModelRevisionPath(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (StringUtils.isBlank(context.getOrg())) {
            return String.join("/", "models", context.getModelName(), context.getRevision());
        }
        return String.join("/", getModelPath(context.getOrg(), context.getModelName()), context.getRevision());
    }

    public static String getModelPath(String organization, String modelName) {
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (StringUtils.isBlank(organization)) {
            return String.join("/", "models", modelName);
        }
        return String.join("/", "models", organization, modelName);
    }

    public static String getLfsUploadEndpoint(String storageId,String repositoryId, String organization, String modelName, String oid) {
        String lfsUploadEndpoint;
        if (repositoryId == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        }
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (oid == null) {
            throw new NullPointerException("oid is marked non-null but is null");
        }
        String repoRelatedPath = String.format("/%s/%s/api/complete_multipart", storageId, repositoryId);
        if (organization == null) {
            lfsUploadEndpoint = String.join("/",repoRelatedPath, modelName, oid);
        } else {
            lfsUploadEndpoint = String.join("/", repoRelatedPath, organization, modelName, oid);
        }
        return lfsUploadEndpoint;
    }

    public static String getLfsTmpUploadDir(String organization, String modelName) {
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        return String.join("/",getModelPath(organization, modelName), "_uploads");
    }

    public static String getLfsTmpUploadPath(String organization, String modelName, String oid) {
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (oid == null) {
            throw new NullPointerException("oid is marked non-null but is null");
        }
        return String.join("/", getLfsTmpUploadDir(organization, modelName), oid);
    }


    public static String sha2(String contentPath) {
        if (contentPath == null) {
            throw new NullPointerException("contentPath is marked non-null but is null");
        }
        DigestInputStream digestInputStream = null;
        try {
            InputStream inputStream = new FileInputStream(contentPath);
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            digestInputStream = new DigestInputStream(inputStream, digest);
            byte[] buffer = new byte[8192];
            while (digestInputStream.read(buffer) != -1) {
                ;
            }
            return encodeHex(digest.digest());
        } catch (Exception e) {
            log.warn("Failed to calculate sha2 for path {}", contentPath);
        } finally {
            StreamUtils.close(digestInputStream);
        }
        return null;
    }


    public static String getGeneratedCommitHash(String revision, String timeStampCommitCreation)  {
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        if (timeStampCommitCreation == null) {
            throw new NullPointerException("timeStampCommitCreation is marked non-null but is null");
        }
        String generatedCommit = sha1(String.join("_", revision, timeStampCommitCreation));
        if (StringUtils.isNotBlank(generatedCommit)) {
            return generatedCommit;
        }
        throw new RuntimeException("Could not generate internal commit sha1");
    }


    private static String sha1(String content) {
        if (content == null) {
            throw new NullPointerException("content is marked non-null but is null");
        }
        DigestInputStream digestInputStream = null;
        try {
            InputStream inputStream = new ByteArrayInputStream(content.getBytes());
            MessageDigest digest = MessageDigest.getInstance("SHA-1");
            digestInputStream = new DigestInputStream(inputStream, digest);
            byte[] buffer = new byte[8192];
            while (digestInputStream.read(buffer) != -1){} ;
            return encodeHex(digest.digest());
        } catch (Exception e) {
            log.warn("Failed to calculate sha1 for content {}", content);
        } finally {
            StreamUtils.close(digestInputStream);
        }
        return null;
    }


    public static String formattedDate() {
        Instant now = Instant.now();
        now = now.truncatedTo(ChronoUnit.MILLIS);
        return DateTimeFormatter.ISO_INSTANT.format(now);
    }


    public static Date convertToDate(String isoInstantString) {
        if (isoInstantString == null) {
            throw new NullPointerException("isoInstantString is marked non-null but is null");
        }
        DateTimeFormatter formatter = DateTimeFormatter.ISO_INSTANT;
        Instant instant = Instant.from(formatter.parse(isoInstantString));
        return Date.from(instant);
    }

    public static String extractSubRevisionFromPath(String path, boolean withOrg) {
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        int subRevisionPosition = withOrg ? 4 : 3;
        String[] tokens = path.split("/");
        if (tokens.length < subRevisionPosition + 1) {
            log.debug("Could not extract sub revision from path '{}' withOrg '{}'", path, Boolean.valueOf(withOrg));
            return null;
        }
        return tokens[subRevisionPosition];
    }

    public static boolean isIsoInstantFormat(String dateString) {
        if (dateString == null) {
            throw new NullPointerException("dateString is marked non-null but is null");
        }
        DateTimeFormatter formatter = DateTimeFormatter.ISO_INSTANT;
        try {
            formatter.parse(dateString);
            return true;
        } catch (DateTimeParseException e) {
            return false;
        }
    }

    private static String encodeHex(byte[] digest) {
        StringBuilder sb = new StringBuilder();
        for (byte b : digest) {
            sb.append(Integer.toString((b & 0xFF) + 256, 16).substring(1));
        }
        return sb.toString();
    }

    public static Multimap<String, String> extractAttributesFromRevisionData(RevisionData dataToSerialize, String repoKey, String org, String modelName, String revision)  {
        String parsedModelName;
        if (dataToSerialize == null) {
            throw new NullPointerException("dataToSerialize is marked non-null but is null");
        }
        if (repoKey == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        }
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        Multimap<String, String> arrayListMultimap = ArrayListMultimap.create();
        if (StringUtils.isNotBlank(dataToSerialize.getModelId())) {
            parsedModelName = dataToSerialize.getModelId();
        } else {
            parsedModelName = modelName;
            log.info("Missing modelId for repo {} organization {} model {} revision {}. Using modelName passed by client.",repoKey, org, modelName, revision);
        }
        arrayListMultimap.put("huggingfaceml.id", parsedModelName);
        if (StringUtils.isNotBlank(dataToSerialize.getSha())) {
            arrayListMultimap.put("huggingfaceml.version", dataToSerialize.getSha());
        }
        if (StringUtils.isNotBlank(dataToSerialize.getAuthor())) {
            arrayListMultimap.put("huggingfaceml.author", dataToSerialize.getAuthor());
        }
        if (StringUtils.isNotBlank(dataToSerialize.getLastModified())) {
            arrayListMultimap.put("huggingfaceml.lastModified", dataToSerialize.getLastModified());
            arrayListMultimap.put("huggingfaceml.generated.revision.sha1",
                    getGeneratedCommitHash(revision, dataToSerialize.getLastModified()));
        }
        if (StringUtils.isNotBlank(dataToSerialize.getLibraryName())) {
            arrayListMultimap.put("huggingfaceml.libraryName", dataToSerialize.getLibraryName());
        }
        CardData cardData = dataToSerialize.getCardData();
        if (cardData != null) {
            if (CollectionUtils.isNotNullOrEmpty(cardData.getTags())) {
                arrayListMultimap.put("huggingfaceml.tags", String.join(",", cardData.getTags()));
            }
            if (CollectionUtils.isNotNullOrEmpty(cardData.getLanguage())) {
                arrayListMultimap.put("huggingfaceml.lang", String.join(",", cardData.getLanguage()));
            }
            if (StringUtils.isNotBlank(cardData.getLicense())) {
                arrayListMultimap.put("huggingfaceml.license", cardData.getLicense());
            }
        }
        return arrayListMultimap;
    }

    private static boolean isValidStringWithPattern(String input, Pattern pattern) {
        if (input == null) {
            throw new NullPointerException("input is marked non-null but is null");
        }
        if (pattern == null) {
            throw new NullPointerException("pattern is marked non-null but is null");
        }
        Matcher matcher = pattern.matcher(input);
        return matcher.matches();
    }

    public static boolean isValidRevisionName(String revision) {
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        return isValidStringWithPattern(revision, REVISION_NAME_PATTERN);
    }

    public static boolean isValidOrganizationName(String organizationName) {
        if (organizationName == null) {
            return true;
        }
        return isValidStringWithPattern(organizationName, ORGANIZATION_NAME_PATTERN);
    }

    public static boolean isValidModelName(String modelName) {
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        return isValidStringWithPattern(modelName, MD_MODEL_NAME_PATTERN);
    }

    public static String getLatestModelInfoPath(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        return String.join("/", getModelPath(context.getOrg(), context.getModelName()), context.getRevision(), ".latest_huggingface_model_info.json");
    }

    public static String getModelInfoPathByTimeStamp(MlModelRequestContext context, String timeStamp) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        return getFilePath(context.getOrg(), context.getModelName(), context.getRevision(), timeStamp, MlModelConstants.LEAD_FILE_NAME);
    }


    public static String getRevisionFolderByTimeStampLeadFilePath(MlModelRequestContext context, String leadFilePath, String timeStampFolder)  {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (leadFilePath == null) {
            throw new NullPointerException("leadFilePath is marked non-null but is null");
        }
        if (timeStampFolder == null) {
            throw new NullPointerException("timeStampFolder is marked non-null but is null");
        }
        if (!PathUtils.isFolderPath(leadFilePath) &&
                PathUtils.getLastPathElement(leadFilePath).equals(MlModelConstants.LEAD_FILE_NAME)) {
            String modelPathFolder = getModelPath(context.getOrg(), context.getModelName());
            String relativeFilePathTimeStamp = String.join("/", timeStampFolder, MlModelConstants.LEAD_FILE_NAME);
            return PathUtils.trimSlashes(leadFilePath.replace(modelPathFolder, "")
                    .replace(relativeFilePathTimeStamp, ""));
        }
        log.error("Tried to extract revision folder from {} but the path is illegal, repoKey: {}, modelId: {}, revision request:{}", leadFilePath, context
                .getRepositoryId(), context.modelId(), context.getRevision());
        throw new RuntimeException("Lead file path is illegal and cannot extract revision folder from it.");
    }


    public static String removeQuote(String etag) {
        return StringUtils.remove(etag, '"');
    }


    public static String extractSizeContentFromResponse(Response response) {
        if (response == null) {
            throw new NullPointerException("response is marked non-null but is null");
        }
        String size = response.getHeaderString("X-Linked-Size");
        if (StringUtils.isBlank(size)) {
            size = response.getHeaderString("Content-Length");
        }
        return size;
    }

}
