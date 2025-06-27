package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@JsonTypeName(GoLayoutProvider.ALIAS)
public class GoRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {
    private List<Map<String,String>> gitVCS;

    public List<Map<String, String>> getGitVCS() {
        return gitVCS;
    }

    public void setGitVCS(List<Map<String, String>> gitVCS) {
        this.gitVCS = gitVCS;
    }

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new GoRepositoryConfigurationData(this);
    }
}
