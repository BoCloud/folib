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
package com.folib.controllers.logging;

import com.folib.booters.PropertiesBooter;
import com.folib.controllers.BaseController;
import com.folib.domain.DirectoryListing;
import com.folib.services.DirectoryListingService;

import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.concurrent.ForkJoinPool;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.folib.util.CommonUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.input.Tailer;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.actuate.autoconfigure.logging.LogFileWebEndpointProperties;
import org.springframework.boot.actuate.logging.LogFileWebEndpoint;
import org.springframework.boot.logging.LogFile;
import org.springframework.core.env.Environment;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import static com.folib.controllers.logging.LoggingManagementController.ROOT_CONTEXT;
import static org.springframework.http.HttpStatus.BAD_REQUEST;

/**
 * This controllers provides a simple wrapper over REST API for the LoggingManagementService.
 *
 * @author Martin Todorov
 * @author Veadan
 * @author Aditya Srinivasan
 * @author veadan
 */
@Controller
@Api(description = "日志管理控制器",tags = "日志管理控制器")
@RequestMapping(ROOT_CONTEXT)
@PreAuthorize("hasAnyAuthority('ADMIN')")
public class LoggingManagementController
        extends BaseController
{

    public final static String ROOT_CONTEXT = "/api/logging";

    @Value("${folib.sse.timeoutMillis:600000}")
    private Long sseTimeoutMillis;

    @Inject
    private PropertiesBooter propertiesBooter;

    @Inject
    private Environment environment;

    @Inject
    private Optional<LogFileWebEndpointProperties> logFileWebEndpointProperties;

    @Inject
    private Function<SseEmitter, SseEmitterAwareTailerListenerAdapter> tailerListenerAdapterPrototypeFactory;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @ApiOperation(value = "Used to download log data.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The log file was retrieved successfully."),
                            @ApiResponse(code = 400, message = "Could not download log data.") })
    @GetMapping(value = "/download/{path:.+}",
                produces = { MediaType.APPLICATION_OCTET_STREAM_VALUE, // forces browser to actually download the file
                             MediaType.TEXT_PLAIN_VALUE,               // plain text / json upon errors
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity downloadLog(@PathVariable("path") String path,
                                      @RequestParam(name = "directory", required = false) String directory,
                                      @RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        try
        {
            if (StringUtils.isNotBlank(directory)) {
                path = directory + File.separator + path;
            }
            Path logsBaseDir = Paths.get(propertiesBooter.getLogsDirectory());
            Path requestedLogPath = Paths.get(logsBaseDir.toString(), path);

            logger.info(String.format("Requested downloading log from path: [%s] resolved to [%s]",
                                       path,
                                       requestedLogPath));

            if (!Files.exists(requestedLogPath))
            {
                return getNotFoundResponseEntity("Requested path does not exist!", accept);
            }
            if (Files.isDirectory(requestedLogPath))
            {
                return getBadRequestResponseEntity("Requested path is a directory!", accept);
            }

            return getStreamToResponseEntity(logFileInputStream(requestedLogPath), FilenameUtils.getName(path));
        }
        catch (IOException e)
        {
            String message = "Could not download log data.";

            return getExceptionResponseEntity(BAD_REQUEST, message, e, accept);
        }
    }

    @ApiOperation(value = "Used to get logs directory.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "The logs directory was retrieved successfully."),
                            @ApiResponse(code = 500, message = "Server error.") })
    @GetMapping(value = { "/browse/{path:.+}" },
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.TEXT_HTML_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public Object browseLogsDirectory(@PathVariable("path") Optional<String> path,
                                      ModelMap model,
                                      HttpServletRequest request,
                                      @RequestHeader(value = HttpHeaders.ACCEPT,
                                                     required = false) String acceptHeader)
    {
        logger.info("Requested directory listing of logs {}/logs/{}", ROOT_CONTEXT, path.orElse(""));

        try
        {
            Path logsBaseDir = Paths.get(propertiesBooter.getLogsDirectory());
            Path requestedLogPath = Paths.get(logsBaseDir.toString(), path.orElse(""));

            logger.info("Requested directory listing of path: [{}] resolved to [{}]", path, requestedLogPath);

            if (!Files.exists(requestedLogPath))
            {
                return getNotFoundResponseEntity("Requested path does not exist!", acceptHeader);
            }
            if (!Files.isDirectory(requestedLogPath))
            {
                return getBadRequestResponseEntity("Requested path is not a directory!", acceptHeader);
            }

            DirectoryListing directoryListing = directoryListingService.fromPath(logsBaseDir, requestedLogPath);
            directoryListing.setFiles(directoryListing.getFiles().stream().filter(file -> file.getName().endsWith(".log") && !file.getName().startsWith("gc-")).collect(Collectors.toList()));
            if (acceptHeader != null && acceptHeader.contains(MediaType.APPLICATION_JSON_VALUE))
            {
                return ResponseEntity.ok(objectMapper.writer().writeValueAsString(directoryListing));
            }

            String currentUrl = StringUtils.chomp(request.getRequestURI(), "/");
            String downloadUrl = currentUrl.replaceFirst("/browse", "/download");
            boolean showBack = path.isPresent() && !StringUtils.isBlank(path.get());
            model.addAttribute("showBack", showBack);
            model.addAttribute("currentUrl", currentUrl);
            model.addAttribute("downloadBaseUrl", downloadUrl);
            model.addAttribute("directories", directoryListing.getDirectories());
            model.addAttribute("files", directoryListing.getFiles());

            return new ModelAndView("directoryListing", model);
        }
        catch (Exception e)
        {
            String message = "Attempt to browse logs failed. Check server logs for more information.";
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, message, e, acceptHeader);
        }
    }

    @ApiOperation(value = "Used to stream logging file.")
    @GetMapping(value = "/stream", produces = { MediaType.TEXT_EVENT_STREAM_VALUE,
                                                  com.folib.net.MediaType.TEXT_PLAIN_UTF8_VALUE })
    public SseEmitter logFileStream()
            throws IOException
    {
        final SseEmitter sseEmitter = new SseEmitter(sseTimeoutMillis);
        Resource logFileResource = getLogFileResource();
        if (logFileResource == null)
        {
            sseEmitter.completeWithError(new IllegalStateException("Missing '" +LogFile.FILE_NAME_PROPERTY+ "' or '" + LogFile.FILE_PATH_PROPERTY + "' properties"));
            return sseEmitter;
        }

        ForkJoinPool forkJoinPool = ForkJoinPool.commonPool();
        forkJoinPool.execute(
                Tailer.create(logFileResource.getFile(),
                              tailerListenerAdapterPrototypeFactory.apply(sseEmitter),
                              1000,
                              true));

        return sseEmitter;
    }

    /**
     * @see LogFileWebEndpoint#logFile()
     */
    private Resource getLogFileResource()
    {
        if (logFileWebEndpointProperties.isPresent() && logFileWebEndpointProperties.get().getExternalFile() != null)
        {
            return new FileSystemResource(logFileWebEndpointProperties.get().getExternalFile());
        }

        LogFile logFile = LogFile.get(environment);

        if (logFile == null)
        {
            logger.info("Missing '" +LogFile.FILE_NAME_PROPERTY+ "' or '" + LogFile.FILE_PATH_PROPERTY + "' properties");
            return null;
        }

        return new FileSystemResource(logFile.toString());
    }

    private InputStream logFileInputStream(Path logFilePath)
            throws IOException
    {
        InputStream stream = new ByteArrayInputStream(CommonUtils.readLastLines(logFilePath, 500).getBytes(StandardCharsets.UTF_8));
        return new BufferedInputStream(stream);
    }



}
