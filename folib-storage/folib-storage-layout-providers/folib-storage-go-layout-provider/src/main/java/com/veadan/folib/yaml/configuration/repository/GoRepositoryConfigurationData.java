package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import java.util.List;
import java.util.Map;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(GoLayoutProvider.ALIAS)
public class GoRepositoryConfigurationData
        extends CustomRepositoryConfiguration {
    private List<Map<String,String>> gitVCS;

    public List<Map<String, String>> getGitVCS() {
        return gitVCS;
    }

    public void setGitVCS(List<Map<String, String>> gitVCS) {
        this.gitVCS = gitVCS;
    }

    public GoRepositoryConfigurationData() {

    }

    public GoRepositoryConfigurationData(final GoRepositoryConfigurationDto delegate) {
        gitVCS = delegate.getGitVCS();
    }

}
