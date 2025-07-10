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
package com.folib.promotion;

/**
 * @author veadan
 * @date 2024/2/10 15:06
 */

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;

public class ChunkStorageService {

    private static final String TEMP_DIR = "/path/to/temp/storage"; // 临时存储目录

    /**
     * 将接收到的数据块存储到文件系统
     *
     * @param chunk 数据块
     * @param fileName 原始文件名
     * @param chunkIndex 数据块索引
     * @throws IOException
     */
    public void storeChunk(byte[] chunk, String fileName, long chunkIndex) throws IOException {
        // 确保临时目录存在
        File tempDir = new File(TEMP_DIR, fileName);
        if (!tempDir.exists()) {
            tempDir.mkdirs();
        }

        // 构建临时文件的路径，包括块索引以确保唯一性
        String chunkFileName = String.format("%s.chunk%d", fileName, chunkIndex);
        File chunkFile = new File(tempDir, chunkFileName);

        // 将块写入文件
        try (FileOutputStream fos = new FileOutputStream(chunkFile)) {
            fos.write(chunk);
        }
    }

    /**
     * 检查是否接收到了所有的块
     *
     * @param totalChunks 总块数
     * @param fileName 文件名
     * @return 是否接收完毕
     */
    public boolean checkAllChunksReceived(long totalChunks, String fileName) {
        File tempDir = new File(TEMP_DIR, fileName);
        // 根据存储的块文件数量判断是否接收完毕
        return tempDir.list().length == totalChunks;
    }

    /**
     * 重组文件
     *
     * @param fileName 文件名
     * @param totalChunks 总块数
     * @throws IOException
     */
    public void reassembleFile(String fileName, long totalChunks) throws IOException {
        File outputFile = new File(TEMP_DIR, fileName + ".reassembled");
        try (FileOutputStream fos = new FileOutputStream(outputFile)) {
            for (int i = 0; i < totalChunks; i++) {
                File chunkFile = new File(TEMP_DIR + "/" + fileName, fileName + ".chunk" + i);
                byte[] chunkData = Files.readAllBytes(chunkFile.toPath());
                fos.write(chunkData);
                // 删除块文件以释放空间
                chunkFile.delete();
            }
        }
        // 可以在这里移动或处理重组后的文件
    }
}
