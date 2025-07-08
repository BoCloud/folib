package com.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * MavenIndexer解析命令类型枚举
 *
 * @author veadan
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum SBOMBinTypeEnum {

    /**
     * Unix AMD
     */
    UNIX_AMD("Unix", "amd", "bin/sbom/linux_amd64/folib"),
    /**
     * Unix ARM
     */
    UNIX_ARM("Unix", "arm", "bin/sbom/linux_arm64/folib"),
    /**
     * Windows
     */
    WINDOWS("Windows", null, "bin/sbom/windows/folib.exe"),
    /**
     * MAC AMD
     */
    MAC_AMD("Mac", "amd", "bin/sbom/darwin_amd64/folib"),
    /**
     * MAC ARM
     */
    MAC_ARM("Mac", "arm", "bin/sbom/darwin_arm64/folib"),
    ;

    /**
     * os
     */
    private String os;
    /**
     * arch
     */
    private String arch;
    /**
     * path
     */
    private String path;

}
