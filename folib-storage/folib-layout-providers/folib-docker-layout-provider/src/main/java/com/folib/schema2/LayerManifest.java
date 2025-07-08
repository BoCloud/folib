package com.folib.schema2;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyDescription;

import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class LayerManifest
{
    @JsonPropertyDescription("The MIME type of the referenced object. This should generally be application/vnd.docker.image.rootfs.diff.tar.gzip. "
            + "Layers of type application/vnd.docker.image.rootfs.foreign.diff.tar.gzip may be pulled from a remote location but they should never be pushed.")
    private String mediaType;
    
    @JsonPropertyDescription("The size in bytes of the object. This field exists "
            + "so that a client will have an expected size for the content before validating. If the length of the retrieved content does not match the specified length, "
            + "the content should not be trusted.")
    private Long size;
    
    @JsonPropertyDescription("The digest of the content, as defined by the Registry V2 HTTP API Specificiation.")
    private String digest;
    
    @JsonPropertyDescription("Provides a list of URLs from which the content may be fetched. Content should be verified against the digest and size. "
            + "This field is optional and uncommon.")
    private List<String> urls;

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

    public List<String> getUrls()
    {
        return urls;
    }

    public void setUrls(List<String> urls)
    {
        this.urls = urls;
    }
}
