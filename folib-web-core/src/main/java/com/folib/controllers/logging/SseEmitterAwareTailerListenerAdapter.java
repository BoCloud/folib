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

import java.io.EOFException;
import java.io.IOException;
import java.util.Objects;

import org.apache.commons.io.input.Tailer;
import org.apache.commons.io.input.TailerListenerAdapter;
import org.apache.commons.lang3.exception.ExceptionUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

/**
 * @author veadan
 */
public class SseEmitterAwareTailerListenerAdapter
        extends TailerListenerAdapter
{

    private static final String FILE_NOT_FOUND_MESSAGE = "File not found";

    private final Logger logger = LoggerFactory.getLogger(getClass());

    protected SseEmitter sseEmitter;

    private Tailer tailer;

    public SseEmitterAwareTailerListenerAdapter(final SseEmitter sseEmitter)
    {
        Objects.requireNonNull(sseEmitter, "sseEmitter cannot be null");
        this.sseEmitter = sseEmitter;
    }

    @Override
    public void init(final Tailer tailer)
    {
        this.tailer = tailer;
    }

    @Override
    public void fileNotFound()
    {
        handleError(FILE_NOT_FOUND_MESSAGE);
    }

    @Override
    public void fileRotated()
    {
        logger.info("File rotated");
        send("rotate", null);
    }

    @Override
    public void handle(final String line)
    {
        send("stream", line);
    }

    @Override
    public void handle(final Exception ex)
    {
        handleError(String.format("Exception occurred [%s]", ExceptionUtils.getStackTrace(ex)));
    }

    private void handleError(final String errorMsg)
    {
        send("error", errorMsg);
        if (sseEmitter != null)
        {
            sseEmitter.completeWithError(new IllegalStateException(errorMsg));
            stopListeningAndCleanupResources();
        }
        logger.error(errorMsg);
    }

    private void send(final String eventName,
                      final String eventData)
    {
        try
        {
            if (sseEmitter != null)
            {
                sseEmitter.send(SseEmitter.event().name(eventName).data(eventData));
            }

        }
        catch (IllegalStateException isEx)
        {
            if (isEx.getMessage().contains("ResponseBodyEmitter is already set complete"))
            {
                stopListeningAndCleanupResources();
            }
        }
        catch (EOFException eofEx)
        {
            stopListeningAndCleanupResources();
        }
        catch (IOException ioEx)
        {
            logger.error(String.format("Unable to send message [%s]", eventData), ioEx);
        }
    }

    private void stopListeningAndCleanupResources()
    {
        if (tailer != null)
        {
            tailer.stop();
            tailer = null;
        }

        try
        {
            if (sseEmitter != null)
            {
                sseEmitter.complete();
            }
        }
        catch (Exception ex)
        {
            // swallow
        }
        sseEmitter = null;
    }
}
