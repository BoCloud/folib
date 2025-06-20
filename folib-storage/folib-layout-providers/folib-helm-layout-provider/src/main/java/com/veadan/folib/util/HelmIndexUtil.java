package com.veadan.folib.util;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.veadan.folib.model.HelmIndexYamlMetadata;
import com.veadan.folib.npm.metadata.Person;
import com.veadan.folib.providers.io.RepositoryPath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

public class HelmIndexUtil {

    private static final Logger logger = LoggerFactory.getLogger(HelmIndexUtil.class);

    public static String redIndexYaml(RepositoryPath repositoryPath) {
        try {
            // 创建 ObjectMapper 实例，并指定使用 YAMLFactory
            ObjectMapper yamlMapper = new ObjectMapper(new YAMLFactory());

            // 指定 YAML 文件路径
            File yamlFile = new File(repositoryPath.getTarget().toString());

            // 将 YAML 文件读取为 HelmIndexYamlMetadata 对象
            HelmIndexYamlMetadata yamlMetadata = yamlMapper.readValue(yamlFile, HelmIndexYamlMetadata.class);

            // 使用 ObjectMapper 将 Java 对象转换为 JSON 字符串
            ObjectMapper jsonMapper = new ObjectMapper();
            jsonMapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);
            return jsonMapper.writeValueAsString(yamlMetadata);
        } catch (Exception e) {
            logger.error("redIndexYaml error", e);
            throw new RuntimeException("redIndexYaml error");
        }
    }

}
