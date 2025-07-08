package com.folib.util;

import com.alibaba.fastjson.JSONObject;

import java.io.BufferedWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * @author veadan
 * @date 2024/3/5
 **/
public class ConcurrentFileWriterUtil {

    private final Path filePath;

    public ConcurrentFileWriterUtil(Path filePath) {
        this.filePath = filePath;
    }

    public void write(String content) throws Exception {
        try (BufferedWriter writer = Files.newBufferedWriter(filePath, StandardCharsets.UTF_8, StandardOpenOption.APPEND)) {
            writer.write(content + System.lineSeparator());
        }
    }
}
