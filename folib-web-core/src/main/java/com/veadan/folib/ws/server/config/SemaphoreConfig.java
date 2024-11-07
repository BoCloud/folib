package com.veadan.folib.ws.server.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

import java.util.concurrent.Semaphore;

@Component
public class SemaphoreConfig {


    @Value("${folib.promotion.thread:4}")
    private int permits;


    public Semaphore getSemaphore() {
        return new Semaphore(permits);
    }
}
