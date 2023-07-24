package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author leipenghui
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConanRecipeInfo {

    /**
     * name
     */
    private String name;

    /**
     * version
     */
    private String version;

    /**
     * user
     */
    private String user;

    /**
     * channel
     */
    private String channel;

    /**
     * reference
     */
    private String reference;

    /**
     * author
     */
    private String author;

    /**
     * license
     */
    private String license;

    /**
     * url
     */
    private String url;

}
