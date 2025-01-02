package com.veadan.folib.domain.migrate;

import lombok.Data;

/**
 * @author huayanjun
 * @since 2024-12-26 16:25
 */
@Data
public class QueueMessage {
    private String migrateId;

    private String storageId;

    private String repositoryId;

}
