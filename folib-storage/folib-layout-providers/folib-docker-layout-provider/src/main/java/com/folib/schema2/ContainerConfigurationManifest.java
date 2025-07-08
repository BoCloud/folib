package com.folib.schema2;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;

/**
 * The config field references a configuration object for a container, by digest. 
 * This configuration item is a JSON blob that the runtime uses to set up the container. 
 * This new schema uses a tweaked version of this configuration to allow image content-addressability on the daemon side.
 * 
 * @author kalski
 *
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ContainerConfigurationManifest
{
    @JsonPropertyDescription("The MIME type of the referenced object. This should generally be application/vnd.docker.container.image.v1+json.")
    private String mediaType;

    @JsonPropertyDescription("The size in bytes of the object. This field exists so that a client will have an expected size for the content before validating. If the length of the retrieved content does not match the specified length, the content should not be trusted.")
    private Long size;
    
    @JsonPropertyDescription("The digest of the content, as defined by the Registry V2 HTTP API Specificiation.")
    private String digest;

    public String getMediaType()
    {
        return mediaType;
    }

    public void setMediaType(String mediaType)
    {
        this.mediaType = mediaType;
    }

    public Long getSize()
    {
        return size;
    }

    public void setSize(Long size)
    {
        this.size = size;
    }

    public String getDigest()
    {
        return digest;
    }

    public void setDigest(String digest)
    {
        this.digest = digest;
    }
}
