package com.folib.enums;

import org.apache.commons.compress.utils.Lists;

import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;
import java.util.List;

/**
 * @ProjectName: folib-server
 * @Package: com.folib.utils
 * @ClassName: DockerHeaderEnum
 * @Author: mac
 * @Description:
 * @Date: 2022/5/27 09:32
 * @Version: 1.0
 */
public enum DockerHeaderEnum {

    DOCKER_DISTRIBUTION_API_VERSION("Docker-Distribution-Api-Version", "registry/2.0"),
    DOCKER_CONTENT_TYPE("content-type", "application/vnd.docker.distribution.manifest.v2+json"),
    DOCKER_UPLOAD_UUID("docker-upload-uuid"),
    DOCKER_CONTENT_DIGEST("docker-content-digest"),
    DOCKER_RATE_LIMIT_SOURCE("docker-ratelimit-source"),
    ETAG("etag"),
    DATE("Date"),
    RANGE("Range"),
    LOCATION("Location"),
    CONTENT_LENGTH("Content-Length"),
    CONTENT_RANGE("Content-Range"),
    RATE_LIMIT_LIMIT("ratelimit-limit"),
    RATE_LIMIT_REMAINING("ratelimit-remaining"),
    STRICT_TRANSPORT_SECURITY("strict-transport-security"),
    STREAM_CONTENT_TYPE("content-type", "application/octet-stream"),
    ACCEPT_JSON("accept", "application/json"),
    ACCEPT_OCI_MANIFEST_V1_JSON("accept", "application/vnd.oci.image.manifest.v1+json"),
    ACCEPT_MANIFEST_V2_JSON("accept", "application/vnd.docker.distribution.manifest.v2+json"),
    ACCEPT_MANIFEST_LIST_V2_JSON("accept", "application/vnd.docker.distribution.manifest.list.v2+json"),
    ACCEPT_OCI_INDEX_V1_JSON("accept", "application/vnd.oci.image.index.v1+json"),
    ACCEPT_MANIFEST_PRETTY("accept", "application/vnd.docker.distribution.manifest.v1+prettyjws"),
    ;

    private String value;
    private final String key;

    DockerHeaderEnum(String key, String value) {
        this.value = value;
        this.key = key;
    }

    DockerHeaderEnum(String key) {
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

    public static MultivaluedMap<String, Object> acceptHeaders() {
        MultivaluedMap<String, Object> acceptHeaders = new MultivaluedHashMap();
        List<Object> acceptList = Lists.newArrayList();
        for (DockerHeaderEnum dockerHeaderEnum : DockerHeaderEnum.values()) {
            if (ACCEPT_JSON.key.equals(dockerHeaderEnum.key)) {
                acceptList.add(dockerHeaderEnum.value);
            }
        }
        acceptHeaders.put(ACCEPT_JSON.key, acceptList);
        return acceptHeaders;
    }

}
