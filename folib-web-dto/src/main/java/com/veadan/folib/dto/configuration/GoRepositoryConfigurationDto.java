package com.veadan.folib.dto.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;

import java.util.List;
import java.util.Map;

/**
 * @author veadan
 * @date 1/15/2024 21:57
 */
@JsonTypeName("go")
public class GoRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {

    private List<Map<String,String>> gitVCS;

    public List<Map<String, String>> getGitVCS() {
        return gitVCS;
    }

    public void setGitVCS(List<Map<String, String>> gitVCS) {
        this.gitVCS = gitVCS;
    }

    @Override
    public <T> T accept(CustomRepositoryConfigurationDtoVisitor<T> visitor) {
        return visitor.visit(this);
    }
}
