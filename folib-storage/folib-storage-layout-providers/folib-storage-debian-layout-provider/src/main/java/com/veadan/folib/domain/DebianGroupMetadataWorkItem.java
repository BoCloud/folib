package com.veadan.folib.domain;

import com.veadan.folib.storage.repository.Repository;
import lombok.Data;

/**
 * @author huayanjun
 * @since 2025-03-06 08:52
 */
@Data
public class DebianGroupMetadataWorkItem {
    private final Repository repository;
    private final String distribution;
    private final String component;
    private final String passphrase;

    public DebianGroupMetadataWorkItem(Repository repository, String distribution, String component, String passphrase) {
        this.repository = repository;
        this.distribution = distribution;
        this.component = component;
        this.passphrase = passphrase;
    }

    public DebianGroupMetadataWorkItem(Repository repository, String distribution, String component) {
        this.repository = repository;
        this.distribution = distribution;
        this.component = component;
        this.passphrase = null;
    }

    public String getUniqueKey() {
        return this.distribution != null ? this.repository.getId() + "/" + this.distribution : this.repository.getId();
    }
}
