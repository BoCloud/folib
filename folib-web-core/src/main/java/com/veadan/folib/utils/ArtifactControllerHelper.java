package com.veadan.folib.utils;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.exception.ExceptionHandlingOutputStream;
import com.veadan.folib.io.ByteRangeInputStream;
import com.veadan.folib.io.StreamUtils;
import com.veadan.folib.providers.io.RepositoryFileAttributes;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import org.carlspring.commons.http.range.ByteRange;
import org.carlspring.commons.http.range.ByteRangeHeaderParser;
import org.carlspring.commons.http.range.validation.ByteRangeValidationException;

import javax.servlet.http.HttpServletResponse;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;

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
                    logger.debug("Received request for a partial download with a single range.");
                    handlePartialDownloadWithSingleRange(is, ranges.get(0), response);
                }
                else
                {
                    logger.debug("Received request for a partial download with multiple ranges.");
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
                                              RepositoryPath path)
            throws IOException
    {
        if (path == null || Files.notExists(path) || Files.isDirectory(path))
        {
            response.setStatus(HttpStatus.NOT_FOUND.value());
            return;
        }
        RepositoryFileAttributes fileAttributes = Files.readAttributes(path, RepositoryFileAttributes.class);

        response.setHeader(HttpHeaders.CONTENT_LENGTH, String.valueOf(fileAttributes.size()));
        response.setHeader(HttpHeaders.LAST_MODIFIED, DateTimeFormatter.RFC_1123_DATE_TIME.format(
                ZonedDateTime.ofInstant(fileAttributes.lastModifiedTime().toInstant(), ZoneId.systemDefault())));

        // TODO: This is far from optimal and will need to have a content type approach at some point:
        String contentType = getContentType(path);
        response.setContentType(contentType);

        response.setHeader(HttpHeaders.ACCEPT_RANGES, "bytes");

        path.getFileSystem().provider().resolveChecksumPathMap(path).forEach((key, value) -> {
            String checksumValue;
            try
            {
                checksumValue = new String(Files.readAllBytes(value), StandardCharsets.UTF_8).trim();
            }
            catch (IOException ioe)
            {
                return;
            }

            String checksumName = String.format("Checksum-%s",
                                                key.toUpperCase().replace("-", ""));

            response.setHeader(checksumName, checksumValue);
        });
    }

    private static String getContentType(RepositoryPath path)
            throws IOException
    {
        if (RepositoryFiles.isChecksum(path) || (path.getFileName().toString().endsWith(".properties")))
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
            // todo docker repository v2
        } else if (path.toString().indexOf("/manifest/") > -1 && path.getFileName().toString().startsWith("sha256:")) {
            return "application/vnd.docker.distribution.manifest.v2+json";
        } else if (path.toString().indexOf("/blobs/") > -1 && path.getFileName().toString().startsWith("sha256:")) {
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

}
