package com.veadan.folib.util;

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
        char[] hexArray = "0123456789ABCDEF".toCharArray();
        char[] hexChars = new char[hash.length * 2];
        for (int i = 0; i < hash.length; i++) {
            int v = hash[i] & 0xFF;
            hexChars[i * 2] = hexArray[v >>> 4];
            hexChars[i * 2 + 1] = hexArray[v & 0x0F];
        }
        return new String(hexChars);
    }
}
