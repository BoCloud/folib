package com.veadan.folib.schema2;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * The config field references a configuration object for a container, by digest. 
 * This configuration item is a JSON blob that the runtime uses to set up the container. 
 * This new schema uses a tweaked version of this configuration to allow image content-addressability on the daemon side.
 * 
 * @author kalski
 *
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Manifests
{
    @JsonPropertyDescription("The MIME type of the referenced object. This should generally be application/vnd.docker.container.image.v1+json.")
    private String mediaType;

    @JsonPropertyDescription("The size in bytes of the object. This field exists so that a client will have an expected size for the content before validating. If the length of the retrieved content does not match the specified length, the content should not be trusted.")
    private Integer size;
    
    @JsonPropertyDescription("The digest of the content, as defined by the Registry V2 HTTP API Specificiation.")
    private String digest;

    /**
     * 平台
     */
    private Platform platform;
}
