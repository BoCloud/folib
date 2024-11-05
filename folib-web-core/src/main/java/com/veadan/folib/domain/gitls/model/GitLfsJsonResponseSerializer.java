package com.veadan.folib.domain.gitls.model;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.google.common.collect.Maps;

import java.io.IOException;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

public class GitLfsJsonResponseSerializer extends JsonSerializer<GitLfsJson> {
    public void serialize(GitLfsJson value, JsonGenerator jgen, SerializerProvider provider) throws IOException {
        jgen.writeStartObject();
        if (StringUtils.isNotBlank(value.getOid()))
            jgen.writeStringField("oid", value.getOid());
        if (value.getSize() != null)
            jgen.writeNumberField("size", value.getSize().longValue());
        if (value.getError() != null)
            jgen.writeObjectField("error", value.getError());
        if (hasHypermediaFields(value)) {
            jgen.writeObjectFieldStart("actions");
            writeHypermediaFieldIfNeeded("self", value.getSelfLink(), Maps.newHashMap(), jgen);
            writeHypermediaFieldIfNeeded("download", value.getDownloadLink(), value.getDownloadHeaders(), jgen);
            writeHypermediaFieldIfNeeded("upload", value.getUploadLink(), value.getUploadHeaders(), jgen);
            writeHypermediaFieldIfNeeded("verify", value.getVerifyLink(), value.getVerifyHeaders(), jgen);
            jgen.writeEndObject();
        }
        jgen.writeEndObject();
    }

    private void writeHypermediaFieldIfNeeded(String fieldName, String value, Map<String, String> headers, JsonGenerator jgen) throws IOException {
        if (StringUtils.isNotBlank(value)) {
            jgen.writeObjectFieldStart(fieldName);
            jgen.writeStringField("href", value);
            writeHeadersIfNeeded(headers, jgen);
            jgen.writeEndObject();
        }
    }

    private void writeHeadersIfNeeded(Map<String, String> headers, JsonGenerator jgen) throws IOException {
        if (!headers.isEmpty()) {
            jgen.writeObjectFieldStart("header");
            for (Map.Entry<String, String> header : headers.entrySet())
                jgen.writeStringField(header.getKey(), header.getValue());
            jgen.writeEndObject();
        }
    }

    private boolean hasHypermediaFields(GitLfsJson response) {
        return (StringUtils.isNotBlank(response.getSelfLink()) || StringUtils.isNotBlank(response.getDownloadLink()) ||
                StringUtils.isNotBlank(response.getUploadLink()) || StringUtils.isNotBlank(response.getVerifyLink()));
    }
}

