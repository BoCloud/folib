package com.folib.constants;

/**
 * @author veadan
 * @date 2024/6/13
 **/
public interface PubConstants {

    /**
     * pub 包索引文件存储路径
     */
    String PACKAGE_JSON_PATH = ".pub/";

    /**
     * pub 包索引文件后缀
     */
    String PACKAGE_JSON_EXTENSION = ".json";

    /**
     * pub Content-Type
     */
    String CONTENT_TYPE = "application/vnd.pub.v2+json";

    /**
     * pubspec.yaml
     */
    String PUB_SPEC_YAML = "pubspec.yaml";

    /**
     * getFinalizeDeploymentResult
     */
    String GET_FINALIZE_DEPLOYMENT_RESULT = "{\n\t\"success\": {\n\t\t\"message\": \"Package was uploaded\"\n\t}\n}";
}
