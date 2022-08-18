package com.veadan.folib.scanner.service;

import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.owasp.dependencycheck.Engine;
import org.owasp.dependencycheck.data.nvdcve.DatabaseProperties;
import org.owasp.dependencycheck.exception.ExceptionCollection;
import org.owasp.dependencycheck.exception.ReportException;
import org.owasp.dependencycheck.reporting.ReportGenerator;
import org.owasp.dependencycheck.utils.Settings;

import java.io.File;
import java.util.Arrays;

import static org.owasp.dependencycheck.reporting.ReportGenerator.getReportFile;

@Slf4j
public class XpEngine extends Engine {
    public XpEngine(@NotNull Settings settings) {
        super(settings);
    }

    public XpEngine(@NotNull Mode mode, @NotNull Settings settings) {
        super(mode, settings);
    }

    public XpEngine(@NotNull ClassLoader serviceClassLoader, @NotNull Settings settings) {
        super(serviceClassLoader, settings);
    }

    public XpEngine(@NotNull ClassLoader serviceClassLoader, @NotNull Mode mode, @NotNull Settings settings) {
        super(serviceClassLoader, mode, settings);
    }

    //执行报告并返回JSON报文
    public File getReport(String applicationName, File outputDir, String format, ExceptionCollection exceptions) throws ReportException {
        return this.getReport(applicationName, (String)null, (String)null, (String)null, outputDir, format, exceptions);
    }

    //执行报告并返回JSON报文
    public File getReport(String applicationName, @Nullable String groupId, @Nullable String artifactId, @Nullable String version, @NotNull File outputDir, String format, ExceptionCollection exceptions) throws ReportException {
        if (this.getMode() == Mode.EVIDENCE_COLLECTION) {
            throw new UnsupportedOperationException("Cannot generate report in evidence collection mode.");
        } else {
            DatabaseProperties prop = this.getDatabase().getDatabaseProperties();

            ReportGenerator r = new ReportGenerator(applicationName, groupId, artifactId, version, Arrays.asList(getDependencies()), this.getAnalyzers(), prop, this.getSettings(), exceptions);
            r.write(outputDir.getAbsolutePath(),format);
            File file = getReportFile(outputDir.getAbsolutePath(), ReportGenerator.Format.valueOf(format.toUpperCase()));
            return file;
        }
    }



}
