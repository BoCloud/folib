package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * @author veadan
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ConanPackageInfo {

    /**
     * settings
     */
    private Map<String, String> settings;

    /**
     * options
     */
    private Map<String, String> options;

    /**
     * requires
     */
    private Map<String, String> requires;

}
