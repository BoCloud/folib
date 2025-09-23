package com.folib.nugetv3.model.index;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonPropertyOrder({"@type", "@id", "clientVersion", "comment"})
public class Resource {
    @JsonProperty("@type")
    private String type;

    @JsonProperty("@id")
    private String id;

    private String comment;

    private String clientVersion;

    public Resource clone() {
        return Resource.builder()
                .type(this.type)
                .id(this.id)
                .comment(this.comment)
                .clientVersion(this.clientVersion)
                .build();
    }
}
