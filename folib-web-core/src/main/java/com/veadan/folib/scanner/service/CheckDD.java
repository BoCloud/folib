package com.veadan.folib.scanner.service;

import cn.hutool.core.io.FileUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.owasp.dependencycheck.exception.ReportException;
import org.owasp.dependencycheck.utils.Settings;

import java.io.File;

@Slf4j
public class CheckDD {
    private static final File REPORT_DIR = new File("target/test-scan-agent/report/");

    public static void beforeClass() {
        if (!REPORT_DIR.exists()) {
            REPORT_DIR.mkdirs();
        }
    }

    public static void main(String[] args) throws Exception {
//        XpEngine engine = new XpEngine(getSettings());
//
//        engine.scan("/Users/leipenghui/Downloads/temp/java.jar");
//        engine.analyzeDependencies();
//        try {
//
//            engine.getReport("folib", new File("/Users/leipenghui/Downloads/folib1.html"), "HTML", null);
//        } catch (ReportException e) {
//            e.printStackTrace();
//        }
    }

    private static Settings getSettings() {
        Settings settings = new Settings();
        settings.setString(Settings.KEYS.DB_CONNECTION_STRING, "jdbc:mysql://xpboot.cn:3306/folib_scanner?useSSL=false&allowPublicKeyRetrieval=true");
        settings.setString(Settings.KEYS.DB_USER, "root");
        settings.setString(Settings.KEYS.DB_DRIVER_NAME, "com.mysql.cj.jdbc.Driver");
        settings.setString(Settings.KEYS.DB_PASSWORD, "199088926");
        settings.setBoolean(Settings.KEYS.AUTO_UPDATE, false);
        settings.setBoolean(Settings.KEYS.PRETTY_PRINT, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_CENTRAL_ENABLED, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_NEXUS_ENABLED, false);
        settings.setBoolean(Settings.KEYS.ANALYZER_NODE_PACKAGE_ENABLED, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_RETIREJS_ENABLED, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_PYTHON_PACKAGE_ENABLED, true);
        settings.setBoolean(Settings.KEYS.ANALYZER_PYTHON_DISTRIBUTION_ENABLED, true);
        settings.setString(Settings.KEYS.ANALYZER_RETIREJS_REPO_JS_URL, "http://110.40.184.113:9998/jsrepository.json");
        settings.setString(Settings.KEYS.CVE_BASE_JSON, "http://110.40.184.113:9998/nvdcve-1.1-%d.json.gz");
        settings.setString(Settings.KEYS.CVE_MODIFIED_JSON, "http://110.40.184.113:9998/feeds/json/cve/1.1/nvdcve-1.1-modified.json.gz");
        return settings;
    }

}
