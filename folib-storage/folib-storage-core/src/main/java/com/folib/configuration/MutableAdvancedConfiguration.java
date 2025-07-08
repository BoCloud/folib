package com.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @author veadan
 * @date 2023/9/24
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MutableAdvancedConfiguration implements Serializable {

    private boolean allowAnonymous = true;

    private boolean showChecksum = false;

    private String globalS3Bucket;
}
