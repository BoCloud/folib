package com.veadan.folib.util;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * @author leipenghui
 * @date 2022/10/19
 **/
public class FileSizeConvertUtils {

    /**
     * 文件大小转换
     * @param sizeInBytes 字节大小
     * @return B、KB、MB、GB
     */
    public static String convert(Long sizeInBytes) {
        BigDecimal bigDecimal = BigDecimal.valueOf(sizeInBytes);
        String size = "";
        double kb = 1024;
        double mb = 1024 * 1024;
        double gb = 1024 * 1024 * 1024;
        double bSize = 0.1 * kb;
        double kbSize = 0.1 * mb;
        double mbSize = 0.1 * gb;
        if (sizeInBytes < bSize) {
            //如果小于0.1KB转化成B
            size = bigDecimal.setScale(2, RoundingMode.HALF_UP) + "B";
        } else if (sizeInBytes < kbSize) {
            //如果小于0.1MB转化成KB
            size = bigDecimal.divide(BigDecimal.valueOf(kb), 2, RoundingMode.HALF_UP) + "KB";
        } else if (sizeInBytes < mbSize) {
            //如果小于0.1GB转化成MB
            size = bigDecimal.divide(BigDecimal.valueOf(mb), 2, RoundingMode.HALF_UP) + "MB";
        } else {
            //其他转化成GB
            size = bigDecimal.divide(BigDecimal.valueOf(gb), 2, RoundingMode.HALF_UP) + "GB";
        }
        return size;
    }
}
