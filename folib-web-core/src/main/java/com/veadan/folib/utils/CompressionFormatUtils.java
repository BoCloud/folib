package com.veadan.folib.utils;

import java.util.Arrays;

/**
 * 解析压缩包格式工具类
 *
 **/
public class CompressionFormatUtils {

    public static boolean isZipFile(byte[] bytes) {
        byte[] magicNumber = {0x50, 0x4B, 0x03, 0x04};
        return Arrays.equals(Arrays.copyOf(bytes, magicNumber.length), magicNumber);
    }

    public static boolean isGzipFile(byte[] bytes) {
        byte[] magicNumber = {0x1F, (byte) 0x8B};
        return Arrays.equals(Arrays.copyOf(bytes, magicNumber.length), magicNumber);
    }

    public static boolean isRarFile(byte[] bytes) {
        byte[] magicNumberRar4 = {0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x00};
        byte[] magicNumberRar5 = {0x52, 0x61, 0x72, 0x21, 0x1A, 0x07, 0x01, 0x00};
        return Arrays.equals(Arrays.copyOf(bytes, magicNumberRar4.length), magicNumberRar4) ||
                Arrays.equals(Arrays.copyOf(bytes, magicNumberRar5.length), magicNumberRar5);
    }

    public static boolean isTarFile(byte[] bytes) {
        // TAR文件没有固定的魔数，但它的文件头包含特定格式的信息
        // 文件头的257-262字节是"ustar"或"ustar\0"标识
        byte[] ustar = {0x75, 0x73, 0x74, 0x61, 0x72};  // "ustar"
        byte[] ustar_null = {0x75, 0x73, 0x74, 0x61, 0x72, 0x00};  // "ustar\0"
        return Arrays.equals(Arrays.copyOfRange(bytes, 257, 262), ustar) ||
                Arrays.equals(Arrays.copyOfRange(bytes, 257, 263), ustar_null);
    }
}
