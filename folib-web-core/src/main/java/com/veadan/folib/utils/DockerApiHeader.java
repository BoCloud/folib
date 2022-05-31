package com.veadan.folib.utils;

import org.jvnet.hk2.annotations.Contract;
import org.springframework.http.HttpStatus;

/**
 * @ProjectName: folib-server
 * @Package: com.veadan.folib.utils
 * @ClassName: DockerApiHeader
 * @Author: mac
 * @Description:
 * @Date: 2022/5/27 09:32
 * @Version: 1.0
 */
public enum DockerApiHeader {

    DOCKER_DISTRIBUTION_API_VERSION("docker-distribution-api-version", "registry/2.0"),
    DOCKER_CONTENT_TYPE("content-type","application/vnd.docker.distribution.manifest.list.v2+json"),
    DOCKER_UPLOAD_UUID("docker-upload-uuid"),
    DOCKER_CONTENT_DIGEST("docker-content-digest"),
    DOCKER_RATELIMIT_SOURCE("docker-ratelimit-source"),
    ETAG("etag"),
    DATE("Date"),
    RANGE("Range"),
    LOCATION("Location"),
    CONTENT_LENGTH("Content-Length"),
    CONTENT_RANGE("Content-Range"),
    RATELIMIT_LIMIT("ratelimit-limit"),
    RATELIMIT_REMAINING("ratelimit-remaining"),
    STRICT_TRANSPORT_SECURITY("strict-transport-security");


    private String value;
    private final String key;

     DockerApiHeader(String key, String value) {
        this.value = value;
        this.key = key;
    }

     DockerApiHeader(String key) {
        this.key = key;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String value() {
        return this.value;
    }

    public String key() {
        return this.key;
    }

}
