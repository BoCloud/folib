package com.veadan.folib.controllers.logging;

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
