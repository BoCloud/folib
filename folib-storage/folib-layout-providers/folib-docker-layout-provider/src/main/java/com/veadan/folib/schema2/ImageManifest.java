package com.veadan.folib.schema2;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * The image manifest provides a configuration and a set of layers for a container image.
 * It’s the direct replacement for the schema-1 manifest.
 *
 * @author kalski
 *
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ImageManifest
{
    @JsonPropertyDescription("This field specifies the image manifest schema version as an integer. This schema uses version 2.")
    private Integer schemaVersion;

    @JsonPropertyDescription("The MIME type of the manifest. This should be set to application/vnd.docker.distribution.manifest.v2+json.")
    private String mediaType;

    @JsonPropertyDescription("Configuration object for a container.")
    private ContainerConfigurationManifest config;

    @JsonPropertyDescription("The layer list is ordered starting from the base image (opposite order of schema1).")
    private List<LayerManifest> layers;

    /**
     * manifests 多架构
     */
    private List<Manifests> manifests;

    private String digest;
}
