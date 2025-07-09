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
package com.folib.domain.gitls.utils;

import com.folib.domain.gitls.model.GitLfsCreateLock;
import com.folib.domain.gitls.constants.GitLfsConstants;
import com.folib.domain.gitls.model.GitLfsJson;
import com.folib.domain.gitls.model.GitLfsLock;
import com.folib.domain.gitls.model.GitLfsName;

import java.net.URLDecoder;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;

import com.folib.entity.GitLfsLockEntity;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.http.ResponseEntity;

public class GitLfsHelper {
    public static final String AUTH_REQUIRED_MESSAGE = "Authorization Required";

    public static final String NOT_FOUND_MESSAGE = "Not Found";

    public static final String FILE_IS_LOCKED_MESSAGE = "File is locked";



    public static final String NO_DEPLOY_MESSAGE = "You do not have deploy permissions for the requested path";

    public static String getOidPath(String oid) {
        return java.lang.String.join("/",  "objects", oid.substring(0, 2), oid.substring(2, 4), oid );
    }

    public static String getArtifactLfsUrl(String baseUrl, String storageId,String repositoryId, String oidPath,  String oid) {
        return java.lang.String.join(GitLfsConstants.PATH_SEPARATOR, baseUrl, storageId,repositoryId, oidPath );
    }

    public static void addChecksumVerificationHeader(Map<String, String> headers, String oid) {
        headers.put("X-Checksum-Sha256", oid);
    }

    public static void addAuthHeaderIfPresent(Map<String, String> headers, @Nullable String authHeader) {
        if (authHeader != null) {
            if (StringUtils.isNotBlank(authHeader)) {
                headers.put("Authorization", authHeader);
            }
        }
    }

    public static ResponseEntity<?> sendAuthChallenge(Logger log) {
        log.debug("Anonymous user trying to deploy without permissions - sending auth challenge");
        return ResponseEntity.status(401).header("WWW-Authenticate", "Basic realm=\"Artifactory Realm\"").body("Authorization Required")
                ;
    }

    public static String getLockFilePath(GitLfsCreateLock createLockJson)  {
        String encodedLockRef;
        if (StringUtils.isEmpty(createLockJson.getPath())) {
            throw new RuntimeException("path cannot be empty");
        }
        String encodedLockPath = URLEncoder.encode(createLockJson.getPath(), StandardCharsets.UTF_8);
        if (createLockJson.getRef() != null && StringUtils.isNotEmpty(createLockJson.getRef().getName())) {
            encodedLockRef = java.lang.String.join(GitLfsConstants.PATH_SEPARATOR,  URLEncoder.encode(createLockJson.getRef().getName(), StandardCharsets.UTF_8) );
        } else {
            encodedLockRef = "lock";
        }
        return java.lang.String.join(GitLfsConstants.PATH_SEPARATOR, ".folib/locks", encodedLockPath, encodedLockRef );
    }

    public static String getRFC3339FormattedDate(long timestamp) {
        SimpleDateFormat rfc3339DateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
        return rfc3339DateFormat.format(new Date(timestamp));
    }


    public static GitLfsName getLockRef( String lockPath) {
        if (lockPath == null) {
            throw new NullPointerException("lockPath is marked non-null but is null");
        }
        Path path = Paths.get(lockPath);
        if (path.getNameCount() <= 3) {
            return null;
        }
        String ref = path.getFileName().toString();
        if ("lock".equals(ref)) {
            return null;
        }
        return new GitLfsName(URLDecoder.decode(ref, StandardCharsets.UTF_8));
    }

    public static String getLockPath( String lockArtifactPath) {
        if (lockArtifactPath == null) {
            throw new NullPointerException("lockArtifactPath is marked non-null but is null");
        }
        Path path = Paths.get(lockArtifactPath);
        if (path.getNameCount() <= 2) {
            return null;
        }
        String lockPath = path.getParent().getFileName().toString();
        return URLDecoder.decode(lockPath, StandardCharsets.UTF_8);
    }


    public static GitLfsLock readLockFromArtifact(GitLfsLockEntity entity) {
        if (entity == null) {
            throw new NullPointerException("artifact is marked non-null but is null");
        }
        return GitLfsLock.builder()
                .path(entity.getPath())
                .id(entity.getId())
                .lockedAt(getRFC3339FormattedDate(entity.getLockedAt()))
                .owner(new GitLfsName(entity.getOwner()))
                .ref(new GitLfsName(entity.getRef()))
                .build();
    }
    public static String getNextCursor(int cursor, int limit, List<GitLfsLock> locks) {
        String nextCursor = null;
        if (limit > 0 && !locks.isEmpty() && locks.size() == limit) {
            cursor += locks.size();
            nextCursor = java.lang.String.valueOf(cursor);
        }
        return nextCursor;
    }

    public static GitLfsJson createLfsDownloadJson(String oid, String storageId,String repositoryId, String oidPath, String authHeader, Long size, String baseUrl) {
        GitLfsJson foundResponse = new GitLfsJson(oid, size);
        foundResponse.setDownloadLink(getArtifactLfsUrl(baseUrl, storageId,repositoryId, oidPath, oid));
        addAuthHeaderIfPresent(foundResponse.getDownloadHeaders(), authHeader);
        return foundResponse;
    }
}

