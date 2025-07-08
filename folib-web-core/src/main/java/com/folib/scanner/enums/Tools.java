package com.folib.scanner.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Getter
public enum Tools {
    /**
     * folib sbom 工具
     */
    FOLIB("folib"),

    /**
     * dependency track
     */
    dependency_track("Dependency-Track"),

    /**
     * qaxoss
     */
    QAXOSS("QAXOSS");

    private String value;
}
