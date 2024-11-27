package com.veadan.folib.services;

import com.veadan.folib.forms.JfrogMigrateForm;
import org.jfrog.artifactory.client.Artifactory;

/**
 * @author huayanjun
 * @since 2024-10-22 17:01
 */
public interface JfrogMigrateService {

    void migrate(Artifactory artifactory,JfrogMigrateForm form);
}
