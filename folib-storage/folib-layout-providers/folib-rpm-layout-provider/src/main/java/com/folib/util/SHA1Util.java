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
package com.folib.util;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class SHA1Util {

    private static final ThreadLocal<MessageDigest> digestThreadLocal = ThreadLocal.withInitial(() -> {
        try {
            return MessageDigest.getInstance("SHA-1");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("Failed to initialize MessageDigest", e);
        }
    });

    public static String getSHA1(Path filePath) {
        try (var in = Files.newInputStream(filePath)) {
            MessageDigest digest = digestThreadLocal.get();
            byte[] buffer = new byte[4096];
            int read;
            while ((read = in.read(buffer)) > 0) {
                digest.update(buffer, 0, read);
            }
            byte[] hash = digest.digest();
            return bytesToHex(hash);
        } catch (IOException e) {
            // 记录日志或进行其他处理
            System.err.println("Error reading file: " + e.getMessage());
            throw new RuntimeException("Error reading file", e);
        }
    }

    private static String bytesToHex(byte[] hash) {
        char[] hexArray = "0123456789abcdef".toCharArray();
        char[] hexChars = new char[hash.length * 2];
        for (int i = 0; i < hash.length; i++) {
            int v = hash[i] & 0xFF;
            hexChars[i * 2] = hexArray[v >>> 4];
            hexChars[i * 2 + 1] = hexArray[v & 0x0F];
        }
        return new String(hexChars);
    }
}
