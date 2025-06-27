package com.veadan.folib.domain.migrate;

import lombok.Data;

import java.util.List;

/**
 * @author veadan
 * @since 2024-12-25 16:32
 */
@Data
public class AddRepositoryForm {

    private String migrateId;

    private List<String> storeAndRepos;

}
