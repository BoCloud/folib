package com.veadan.folib.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class VulnerabilitiesInfo {
    private String id;

    private String appId;

    private String packageName;

    private LocalDateTime insertTime;

    private String packagePath;

    private String bugName;

    private String repairVersion;

    private String report;

}
