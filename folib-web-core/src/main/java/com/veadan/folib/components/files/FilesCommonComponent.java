package com.veadan.folib.components.files;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.veadan.folib.enums.FileUnitTypeEnum;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author leipenghui
 * @date 2024/11/29
 **/
@Slf4j
@Component
public class FilesCommonComponent {

    public void storeContent(String content, String logPath) {
        try {
            if (StringUtils.isBlank(content)) {
                return;
            }
            storeLog(content, logPath);
        } catch (Exception ex) {
            log.error("Store content error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    private void storeLog(String content, String logPath) throws IOException {
        Path writePath = getLogPath(getLatestIndex(logPath), logPath);
        //追加写模式
        try (BufferedWriter writer = Files.newBufferedWriter(writePath, StandardCharsets.UTF_8, StandardOpenOption.APPEND)) {
            writer.write(content + System.lineSeparator());
        }
    }

    private Path getLogPath(Integer index, String logPath) throws IOException {
        if (Objects.isNull(index)) {
            index = 1;
        }
        String filename = DateUtil.format(DateUtil.date(), DatePattern.PURE_DATE_PATTERN) + "_index_%s.txt";
        String filePath = logPath + File.separator + String.format(filename, index);
        log.debug("Log file path [{}]", filePath);
        Path path = Path.of(filePath);
        Files.createDirectories(path.getParent());
        //每个文件10M大小
        BigDecimal maxSize = BigDecimal.valueOf(10);
        if (!Files.exists(path)) {
            Files.createFile(path);
        }
        if (FileSizeConvertUtils.convertBytesWithDecimal(Files.size(path), FileUnitTypeEnum.MB.getUnit()).compareTo(maxSize) >= 0) {
            return getLogPath(index + 1, logPath);
        }
        return path;
    }

    private Integer getLatestIndex(String filePath) {
        Path path = Path.of(filePath);
        if (Files.exists(path)) {
            try (Stream<Path> pathStream = Files.list(path)) {
                List<Path> pathList = pathStream.sorted().collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(pathList)) {
                    Path logPath = pathList.get(pathList.size() - 1);
                    String filename = logPath.getFileName().toString();
                    filename = FilenameUtils.getBaseName(filename);
                    List<String> nameSplitList = Arrays.asList(filename.split("_"));
                    if (CollectionUtils.isNotEmpty(nameSplitList)) {
                        String index = nameSplitList.get(nameSplitList.size() - 1);
                        return Integer.parseInt(index);
                    }
                }
            } catch (Exception ex) {
                log.error(ExceptionUtils.getStackTrace(ex));
            }
        }
        return null;
    }
}
