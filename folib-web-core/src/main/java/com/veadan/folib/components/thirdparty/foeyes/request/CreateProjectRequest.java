package com.veadan.folib.components.thirdparty.foeyes.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author leipenghui
 * @date 2024/4/22
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class CreateProjectRequest {

    /**
     * author
     */
    private String author;

    /**
     * publisher
     */
    private String publisher;

    /**
     * group
     */
    private String group;

    /**
     * name
     */
    private String name;

    /**
     * description
     */
    private String description;

    /**
     * version
     */
    private String version;

    /**
     * classifier
     */
    private String classifier;

    /**
     * cpe
     */
    private String cpe;

    /**
     * purl
     */
    private String purl;

    /**
     * swidTagId
     */
    private String swidTagId;

    /**
     * active
     */
    private Boolean active;

    /**
     * parent
     */
    private ParentRequest parent;

    /**
     * tags
     */
    private List<TagRequest> tags;
}
