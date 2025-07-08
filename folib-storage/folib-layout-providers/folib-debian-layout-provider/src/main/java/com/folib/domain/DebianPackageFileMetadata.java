package com.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author veadan
 * @since 2025-03-05 15:47
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DebianPackageFileMetadata {

    private String comp;
    private String sha1;
    private String arch;
}
