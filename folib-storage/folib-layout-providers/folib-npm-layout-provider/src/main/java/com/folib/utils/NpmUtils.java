package com.folib.utils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.PrettyPrinter;
import com.fasterxml.jackson.core.util.MinimalPrettyPrinter;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.github.zafarkhaja.semver.Version;
import com.folib.artifact.coordinates.NpmCoordinates;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;


/**
 * @author veadan
 * @date 2024/6/14
 **/
@Slf4j
public class NpmUtils {

    private static final ObjectMapper PUB_MAPPER_YAML;

    private static final ObjectMapper PUB_MAPPER_JSON;

    static {
        PUB_MAPPER_JSON = new ObjectMapper();
        PUB_MAPPER_JSON.setDefaultPrettyPrinter((PrettyPrinter) new MinimalPrettyPrinter());
        PUB_MAPPER_JSON.setSerializationInclusion(JsonInclude.Include.NON_NULL);
        PUB_MAPPER_JSON.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        PUB_MAPPER_YAML = new ObjectMapper((JsonFactory) new YAMLFactory());
        PUB_MAPPER_YAML.setDefaultPrettyPrinter((PrettyPrinter) new MinimalPrettyPrinter());
        PUB_MAPPER_YAML.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    public static String getPackageMetadataPath(String packageName) {
        return String.join("/", ".npm", packageName, "package.json");
    }

    public static String getBinaryMetadataPath(String packageName) {
        return String.join("/", ".npm",  packageName, "binary.json");
    }

    public static String getFilePath(String path, String fileName) {
        return String.join("/", path, fileName);
    }

    public static boolean isMetaData(String path) {
        return (path.startsWith(".pub") && path.endsWith(".json"));
    }

    public static boolean isPubFile(String fileName) {
        return fileName.endsWith(".tar.gz");
    }

    public static ObjectMapper getPubYamlObjectMapper() {
        return PUB_MAPPER_YAML;
    }

    public static ObjectMapper getPubJsonObjectMapper() {
        return PUB_MAPPER_JSON;
    }

    public static boolean isNameFieldValid(String value) {
        return (StringUtils.isNotBlank(value) && isNameMatchConvention(value));
    }

    public static boolean isVersionFieldValid(String value) {
        boolean isValidVersion = false;
        try {
            Version.valueOf(value);
            isValidVersion = true;
        } catch (Exception e) {
            log.warn("The version : {} is not a semver version : {}", value, e.getMessage());
        }
        return isValidVersion;
    }

    public static boolean isNameMatchConvention(String text) {
        return NpmCoordinates.NPM_NAME_PATTERN.matcher(text).matches();
    }

    public static boolean isPackageMetadataValidForIndexing(String packageName, String packageVersion) {
        return (isNameFieldValid(packageName) && isVersionFieldValid(packageVersion));
    }
}

