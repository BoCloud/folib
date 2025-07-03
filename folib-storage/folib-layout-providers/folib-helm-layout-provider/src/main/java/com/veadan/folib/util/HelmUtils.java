package com.veadan.folib.util;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.Instant;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.veadan.folib.model.HelmIndexYamlMetadata;
import lombok.Generated;
import lombok.NonNull;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public abstract class HelmUtils {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(HelmUtils.class);

    private final int maxSizeLimit=536870912;
    private static ObjectMapper mapper;

    public static boolean isHelmChartFile(String fileName) {
        return fileName.endsWith(".tgz");
    }

    public static boolean isMetadataFile(String fileName) {
        return fileName.endsWith("index.yaml");
    }

    public static ObjectMapper getYamlObjectMapper() {
        if (mapper == null) {
            mapper = new ObjectMapper((JsonFactory)createYamlFactory()) {
                public byte[] writeValueAsBytes(Object value) throws JsonProcessingException {
                    return writeValueAsString(value).getBytes(StandardCharsets.UTF_8);
                }

                public String writeValueAsString(Object value) throws JsonProcessingException {
                    String output = super.writeValueAsString(value);
                    return HelmVersionUtil.addQuotesToVersionsAttributes(output);
                }
            };
            mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
            mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        }
        return mapper;
    }

    public static HelmIndexYamlMetadata emptyIndexYaml() {
        HelmIndexYamlMetadata indexYaml = new HelmIndexYamlMetadata();
        indexYaml.apiVersion = "v1";
        indexYaml.generated = Instant.now().toString();
        return indexYaml;
    }


    //public static String getChartsBaseUrl(String remoteRepoKey, PackageHandlerArtifactoryConfigService packageHandlerRepoDescriptorService) {
    //    String res = packageHandlerRepoDescriptorService.getStringValue(remoteRepoKey, PackageDescriptorKey.HELM_CHARTS_BASE_URL, null);
    //    if (StringUtils.isBlank(res)) {
    //        res = packageHandlerRepoDescriptorService.getStringValue(remoteRepoKey, PackageDescriptorKey.URL, "");
    //    }
    //    return PathUtils.addTrailingSlash(res);
    //}

    //@Nullable
    //public static String getBaseUrlWithOverrideContextPathAndRepoKey(@Nullable String repoKey, @Nullable HttpServletRequest httpServletRequest, PackageHandlerArtifactoryConfigService packageHandlerRepoDescriptorService) {
    //    String suffix = (repoKey != null) ? ("/" + repoKey) : "";
    //    String res = packageHandlerRepoDescriptorService.getArtifactoryBaseUrl(httpServletRequest);
    //    if (StringUtils.isNotBlank(res)) {
    //        return PathUtils.trimTrailingSlashes(res) + PathUtils.trimTrailingSlashes(res);
    //    }
    //    return null;
    //}

    //public static String getHelmContextUrlFromPackageRequest(PackageRequestContext packageRequestContext) {
    //    String res = (String)packageRequestContext.getRequestHeaders().get("X-Orig-Client-Uri");
    //    if (StringUtils.isBlank(res)) {
    //        res = packageRequestContext.getServletContextUrl();
    //    }
    //    return res;
    //}

    public static InputStream indexYamlToInputStream(HelmIndexYamlMetadata indexYaml) {
        try {
            indexYaml.generated = Instant.now().toString();
           return new ByteArrayInputStream( writeIndexYaml(indexYaml).getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            log.error("Failed to write index.yaml:{}", e.getMessage());
            log.debug("Failed to write index.yaml:", e);
            throw new RuntimeException(e.getMessage(), e);
        }
    }



    public static String writeIndexYaml(HelmIndexYamlMetadata helmIndexYamlMetadata) throws JsonProcessingException {
        return getYamlObjectMapper().writeValueAsString(helmIndexYamlMetadata);
    }

    private static YAMLFactory createYamlFactory() {
        //LoaderOptions loaderOptions = new LoaderOptions();
        //loaderOptions.setCodePointLimit(536870912);
        return YAMLFactory.builder().configure(YAMLGenerator.Feature.MINIMIZE_QUOTES, true)
                .configure(YAMLGenerator.Feature.ALWAYS_QUOTE_NUMBERS_AS_STRINGS, true)
                .configure(YAMLGenerator.Feature.WRITE_DOC_START_MARKER, false)
                .configure(YAMLGenerator.Feature.WRITE_DOC_START_MARKER, false)
                .configure(YAMLGenerator.Feature.SPLIT_LINES, false)
                //.loaderOptions(loaderOptions)
                .build();
    }

}

