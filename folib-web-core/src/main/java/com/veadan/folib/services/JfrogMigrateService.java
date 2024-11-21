package com.veadan.folib.services;

import com.veadan.folib.forms.JfrogMigrateForm;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author huayanjun
 * @since 2024-10-22 17:01
 */
public interface JfrogMigrateService {

    void migrate(JfrogMigrateForm form);

    void changeRepositoryType(MultipartFile file);
}
