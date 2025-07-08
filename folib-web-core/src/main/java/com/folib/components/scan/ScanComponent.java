package com.folib.components.scan;


import com.folib.constant.GlobalConstants;

import com.folib.providers.io.RepositoryPath;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class ScanComponent {

    /**
     * 写入扫描报告
     *
     * @param repositoryPath 制品
     * @param reportContent 扫描结果
     */
    public void writeReport(RepositoryPath repositoryPath, String reportContent) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return;
        }
        if (StringUtils.isBlank(reportContent)) {
            return;
        }
        RepositoryPath reportRepositoryPath = getReportRepositoryPath(repositoryPath);
        try (BufferedWriter writer = Files.newBufferedWriter(reportRepositoryPath, StandardCharsets.UTF_8, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)) {
            writer.write(reportContent);
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * 读取扫描报告
     *
     * @param repositoryPath 制品
     * @return 扫描报告
     */
    public String readReport(RepositoryPath repositoryPath) {
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return "";
        }
        RepositoryPath reportRepositoryPath = getReportRepositoryPath(repositoryPath);
        if (!Files.exists(reportRepositoryPath)) {
            return "";
        }
        StringBuilder contentBuilder = new StringBuilder();
        try (BufferedReader reader = Files.newBufferedReader(reportRepositoryPath, StandardCharsets.UTF_8)) {
            String line;
            while ((line = reader.readLine()) != null) {
                contentBuilder.append(line).append(System.lineSeparator());
            }
        } catch (Exception ex) {
            log.error("RepositoryPath [{}] read report error [{}]", repositoryPath.toString(), ExceptionUtils.getStackTrace(ex));
            return "";
        }
        return contentBuilder.toString();
    }

    private RepositoryPath getReportRepositoryPath(RepositoryPath repositoryPath) {
        String artifactMetadataDirectoryName = "." + FilenameUtils.getName(repositoryPath.getFileName().toString()) + GlobalConstants.FO_LIBRARY_METADATA;
        return repositoryPath.resolveSibling(artifactMetadataDirectoryName).resolve("report.json");
    }
}
