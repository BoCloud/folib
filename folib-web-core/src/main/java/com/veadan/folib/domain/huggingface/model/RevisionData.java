package com.veadan.folib.domain.huggingface.model;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

import lombok.Data;

@Data
public class RevisionData {
    private String _id;

    private String id;

    private String modelId;

    private String author;

    @JsonProperty(required = true)
    private String sha;

    @JsonProperty("lastModified")
    @JsonAlias({"last_modified"})
    private String lastModified;

    @JsonProperty("private")
    private boolean privateProperty;

    private String disabled;

    private String gated;

    private long downloads;

    private long likes;

    private String pipeline_tag;

    private List<String> tags;

    private String libraryName;

    private RevisionConfig config;

    private TransformersInfo transformersInfo;

    private CardData cardData;

    private List<SiblingItem> siblings;

}

