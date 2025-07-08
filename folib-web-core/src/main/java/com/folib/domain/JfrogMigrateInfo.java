package com.folib.domain;

import lombok.Data;

/**
 * 在repository 中加一个 frogSync
 * @author veadan
 * @since 2024-12-20 16:56
 */
@Data
public class JfrogMigrateInfo {

    private int totalArtifact;
    private int syncedArtifact;
    /*
        0-待同步
        1-同步中
        2-暂停中
        3-已同步
        4-已更新
     */
    private int status;






}
