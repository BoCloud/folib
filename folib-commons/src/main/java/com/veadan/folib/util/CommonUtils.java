package com.veadan.folib.util;

import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

/**
 * @author leipenghui
 * @date 2022/11/25
 **/
@Slf4j
public class CommonUtils {

    /**
     * 读出文件的最后n行
     *
     * @param path 文件
     * @param num  第几行
     * @return 读取文本文件的最后n行
     */
    public static String readLastLines(Path path, int num) {
        if (num == 0) {
            return "";
        }
        // 判断该文件是否存在，可读
        if (!Files.exists(path) || Files.isDirectory(path) || !Files.isReadable(path)) {
            return "";
        }
        List<String> list = Lists.newArrayList();
        // 行数
        int count = 0;
        RandomAccessFile rafFile = null;
        try {
            // 选择只读模式
            rafFile = new RandomAccessFile(path.toAbsolutePath().toFile(), "r");
            // 读取文件长度
            long length = rafFile.length();
            // 判断长度
            if (length == 0L) {
                return "";
            } else {
                // 因为是倒数，所以从最大值开始读起
                long pos = length - 1;
                // 当下一个大于0，则代表文章有内容
                while (pos > 0) {
                    // 开始读取
                    rafFile.seek(pos);
                    // 如果读取到\n代表是读取到一行
                    if (rafFile.readByte() == '\n') {
                        String line = rafFile.readLine();
                        if (StringUtils.isNotBlank(line)) {
                            line = new String(line.getBytes(StandardCharsets.ISO_8859_1), StandardCharsets.UTF_8);
                            // 保存结果
                            list.add(line);
                            // 行数统计，如果到达了指定的行数，就跳出循环
                            count++;
                        }
                        if (count == num) {
                            break;
                        }
                    }
                    pos--;
                }
                //next为0，代表长度为1
                if (pos == 0) {
                    rafFile.seek(0);
                    list.add(rafFile.readLine());
                }
            }
            Collections.reverse(list);
            return String.join("\n", list);
        } catch (IOException e) {
            log.error("path ：{} readLastLines ：{} error：{}", path, num, ExceptionUtils.getStackTrace(e));
            return "";
        } finally {
            if (rafFile != null) {
                try {
                    rafFile.close();
                } catch (IOException e) {
                    log.error("path ：{} readLastLines ：{} error：{}", path, num, ExceptionUtils.getStackTrace(e));
                }
            }
        }
    }
}
