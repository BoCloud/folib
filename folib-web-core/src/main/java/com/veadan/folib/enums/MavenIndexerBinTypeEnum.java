package com.veadan.folib.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * MavenIndexer解析命令类型枚举
 *
 * @author leipenghui
 **/
@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum MavenIndexerBinTypeEnum {

    /**
     * Unix
     */
    UNIX("Unix", null, "bin/maven-indexer/linux/folib_index_reader_amd64"),
    /**
     * Windows
     */
    WINDOWS("Windows", null, "bin/maven-indexer/windows/folib_index_reader.exe"),
    /**
     * MAC AMD
     */
    MAC_AMD("Mac", "amd", "bin/maven-indexer/mac/folib_index_reader_amd64"),
    /**
     * MAC ARM
     */
    MAC_ARM("Mac", "arm", "bin/maven-indexer/mac/folib_index_reader_arm64"),
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
