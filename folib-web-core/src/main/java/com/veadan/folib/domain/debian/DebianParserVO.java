package com.veadan.folib.domain.debian;

import lombok.Data;

/**
 * @author huayanjun
 * @since 2024-09-06 11:22
 */
@Data
public class DebianParserVO {
    private String version;

    private String fileName;

    private String component;

    private String architecture;

    private String path;

}
