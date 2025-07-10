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

import com.alibaba.fastjson.JSON;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.controllers.BaseController;
import com.folib.enums.ProductTypeEnum;
import com.folib.exception.ExceptionHandlingOutputStream;
import com.folib.io.ByteRangeInputStream;
import com.folib.io.StreamUtils;
import com.folib.providers.io.RepositoryPath;
import com.folib.schema2.ImageManifest;
import com.folib.storage.repository.Repository;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.folib.commons.http.range.ByteRange;
import com.folib.commons.http.range.ByteRangeHeaderParser;
import com.folib.commons.http.range.validation.ByteRangeValidationException;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import static org.springframework.http.HttpStatus.PARTIAL_CONTENT;
import static org.springframework.http.HttpStatus.REQUESTED_RANGE_NOT_SATISFIABLE;

/**
 * @author Veadan
 */
public class ArtifactControllerHelper
{

    public static final String MULTIPART_BOUNDARY = "3d6b6a416f9b5";

    private static final Logger logger = LoggerFactory.getLogger(ArtifactControllerHelper.class);

    private static final String RANGE_REGEX = "^bytes=\\d*-\\d*(,\\d*-\\d*)*$";

    private static final String FULL_FILE_RANGE_REGEX = "^bytes=(0\\/\\*|0-|0)$";

    private static final int DEFAULT_BUFFER_SIZE = 4096;

    private static final String CRLF = "\r\n";

    private ArtifactControllerHelper()
    {
    }

    public static void handlePartialDownload(InputStream is,
                                             HttpHeaders headers,
                                             HttpServletResponse response)
            throws IOException
    {
        String contentRange = headers.getFirst(HttpHeaders.RANGE);
        ByteRangeHeaderParser parser = new ByteRangeHeaderParser(contentRange);

        try
        {
            List<ByteRange> ranges = parser.getRanges();
            if (!CollectionUtils.isEmpty(ranges))
            {
                if (ranges.size() == 1)
                {
                    logger.info("Received request for a partial download with a single range.");
                    handlePartialDownloadWithSingleRange(is, ranges.get(0), response);
                }
                else
                {
                    logger.info("Received request for a partial download with multiple ranges.");
                    handlePartialDownloadWithMultipleRanges(is, ranges, response);
                }
            }
        }
        catch (ByteRangeValidationException e)
        {
            logger.error(e.getMessage(), e);

            ByteRangeInputStream bris = StreamUtils.findSource(ByteRangeInputStream.class, is);
            long length = bris != null ? StreamUtils.getLength(bris) : 0;
            setRangeNotSatisfiable(response, length);
        }
    }

    private static void handlePartialDownloadWithSingleRange(InputStream is,
                                                             ByteRange byteRange,
                                                             HttpServletResponse response)
            throws IOException
    {
        ByteRangeInputStream bris = StreamUtils.findSource(ByteRangeInputStream.class, is);
        long inputLength = bris != null ? StreamUtils.getLength(bris) : 0;

        if (byteRange.getOffset() < inputLength)
        {
            StreamUtils.setCurrentByteRange(bris, byteRange);

            prepareResponseBuilderForPartialRequestWithSingleRange(byteRange, inputLength, response);

            BaseController.copyToResponse(is, response);
        }
        else
        {
            setRangeNotSatisfiable(response, inputLength);
        }
    }

    private static void handlePartialDownloadWithMultipleRanges(InputStream is,
                                                                List<ByteRange> byteRanges,
                                                                HttpServletResponse response)
            throws IOException
    {
        ByteRangeInputStream bris = StreamUtils.findSource(ByteRangeInputStream.class, is);
        long length = bris != null ? StreamUtils.getLength(bris) : 0;

        boolean anyByteRangeNotSatisfiable = byteRanges.stream()
                                                       .anyMatch(byteRange -> byteRange.getOffset() >= length);

        if (anyByteRangeNotSatisfiable)
        {
            setRangeNotSatisfiable(response, length);
        }
        else
        {
            final String rangesContentType = response.getContentType();

            prepareResponseBuilderForPartialRequestWithMultipleRanges(response);

            copyPartialMultipleRangeToResponse(is, response, byteRanges, rangesContentType);
        }
    }

    private static void setRangeNotSatisfiable(HttpServletResponse response,
                                               long length)
            throws IOException
    {
        response.setHeader(HttpHeaders.CONTENT_RANGE, "bytes */" + length);
        response.setStatus(REQUESTED_RANGE_NOT_SATISFIABLE.value());
        response.flushBuffer();
    }

    private static void prepareResponseBuilderForPartialRequestWithSingleRange(ByteRange byteRange,
                                                                               long inputLength,
                                                                               HttpServletResponse response)
    {
        String contentRangeHeaderValue = String.format("bytes %d-%d/%d",
                                                       byteRange.getOffset(),
                                                       inputLength - 1L,
                                                       inputLength);

        response.setHeader(HttpHeaders.CONTENT_RANGE, contentRangeHeaderValue);

        response.setStatus(PARTIAL_CONTENT.value());
    }

    private static void prepareResponseBuilderForPartialRequestWithMultipleRanges(HttpServletResponse response)
    {
        response.setContentType("multipart/byteranges; boundary=" + MULTIPART_BOUNDARY);

        response.setStatus(PARTIAL_CONTENT.value());
    }

    public static boolean isRangedRequest(HttpHeaders headers)
    {
        if (headers == null)
        {
            return false;
        }
        else
        {
            String contentRange = headers.getFirst(HttpHeaders.RANGE);
            return contentRange != null && contentRange.matches(RANGE_REGEX) &&
                   !contentRange.matches(FULL_FILE_RANGE_REGEX);
        }
    }

    public static void provideArtifactHeaders(HttpServletResponse response,
                                              RepositoryPath repositoryPath,
                                              Path path)
            throws IOException
    {
        if (path == null || Files.notExists(path) || Files.isDirectory(path))
        {
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return;
        }
        if (setContentLength(response)) {
            response.setHeader(HttpHeaders.CONTENT_LENGTH, String.valueOf(Files.size(path)));
        }
        response.setHeader(HttpHeaders.LAST_MODIFIED, DateTimeFormatter.RFC_1123_DATE_TIME.format(
                ZonedDateTime.ofInstant(Files.getLastModifiedTime(path).toInstant(), ZoneId.systemDefault())));
        Repository repository = repositoryPath.getRepository();
        // TODO: This is far from optimal and will need to have a content type approach at some point:
        String contentType = getContentType(repository, path);
        response.setContentType(contentType);

        response.setHeader(HttpHeaders.ACCEPT_RANGES, "bytes");
    }

    private static String getContentType(Repository repository, Path path)
            throws IOException
    {
        if (path.getFileName().toString().endsWith(".properties"))
        {
            return MediaType.TEXT_PLAIN_VALUE;
        }
        else if (path.getFileName().toString().endsWith("xml"))
        {
            return MediaType.APPLICATION_XML_VALUE;
        }
        else if (path.getFileName().toString().endsWith(".gz"))
        {
            return com.google.common.net.MediaType.GZIP.toString();
        } else if (ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(repository.getLayout()) && DockerCoordinates.isManifestPath(path)) {
            //docker repository v2
            ImageManifest imageManifest = JSON.parseObject(Files.readString(path), ImageManifest.class);
            return imageManifest.getMediaType();
        } else if (ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(repository.getLayout()) && path.toString().contains("/blobs/") && path.getFileName().toString().startsWith("sha256:")) {
            return "application/vnd.docker.image.rootfs.diff.tar.gzip";
        }

        return MediaType.APPLICATION_OCTET_STREAM_VALUE;

    }

    private static void copyPartialMultipleRangeToResponse(InputStream is,
                                                           HttpServletResponse response,
                                                           List<ByteRange> byteRanges,
                                                           String contentType)
            throws IOException
    {
        BufferedInputStream bis = new BufferedInputStream(is, DEFAULT_BUFFER_SIZE);
        long inputLength = Long.parseLong(response.getHeader(HttpHeaders.CONTENT_LENGTH));
        long totalBytes = 0L;

        try (OutputStream os = new ExceptionHandlingOutputStream(response.getOutputStream()))
        {
            for (ByteRange byteRange : byteRanges)
            {
                long start = byteRange.getOffset();
                long end = byteRange.getLimit();
                long length = end - start + 1;

                os.write(toByteArray(""));
                os.write(toByteArray("--" + MULTIPART_BOUNDARY));

                final String contentTypeHeader = String.format("%s: %s",
                                                               HttpHeaders.CONTENT_TYPE,
                                                               contentType);
                os.write(toByteArray(contentTypeHeader));

                final String contentRangeHeader = String.format("%s: bytes %d-%d/%d",
                                                                HttpHeaders.CONTENT_RANGE,
                                                                start,
                                                                end,
                                                                inputLength);
                os.write(toByteArray(contentRangeHeader));

                os.write(toByteArray(""));

                // Check if it is allowed to read the stream more than once.
                if (bis.markSupported())
                {
                    int readLength;
                    byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];
                    long toRead = length;

                    long markLimit = Math.max(toRead, DEFAULT_BUFFER_SIZE) + 1L;
                    int markLimitInt = Math.toIntExact(markLimit);
                    // Needed for reading the stream more than once.
                    bis.mark(markLimitInt);

                    // Skip to the byte range offset.
                    bis.skip(start);

                    while ((readLength = bis.read()) != -1)
                    {
                        toRead -= readLength;

                        // Range length is greater than the buffer length.
                        if (toRead > 0)
                        {
                            os.write(buffer, 0, readLength);
                            os.flush();

                            totalBytes += readLength;
                        }
                        else
                        {
                            os.write(buffer, 0, (int) toRead + readLength);
                            os.flush();

                            totalBytes += (toRead + readLength);
                            break;
                        }
                    }

                    // Needed for reading the stream more than once.
                    bis.reset();
                }
            }

            os.write(toByteArray(""));
            os.write(toByteArray("--" + MULTIPART_BOUNDARY + "--"));
            os.flush();

            response.setHeader(HttpHeaders.CONTENT_LENGTH, Long.toString(totalBytes));
            response.flushBuffer();
        }
    }

    private static byte[] toByteArray(String string)
    {
        return (string.concat(CRLF)).getBytes(StandardCharsets.UTF_8);
    }


    private static boolean setContentLength(HttpServletResponse response) {
        String contentLength = response.getHeader(HttpHeaders.CONTENT_LENGTH);
        if (StringUtils.isBlank(contentLength) || "-1".equalsIgnoreCase(contentLength)) {
            return true;
        }
        return false;
    }
}
