package com.veadan.folib.domain.adapter.jfrog;

import com.veadan.folib.storage.repository.RepositoryDto;
import org.jfrog.artifactory.client.model.Privilege;

import static org.jfrog.artifactory.client.model.Privilege.ADMIN;
import static org.jfrog.artifactory.client.model.Privilege.ANNOTATE;
import static org.jfrog.artifactory.client.model.Privilege.DELETE;
import static org.jfrog.artifactory.client.model.Privilege.DEPLOY;
import static org.jfrog.artifactory.client.model.Privilege.READ;

/**
 * @author huayanjun
 * @since 2024-10-24 10:24
 */
public class JfrogMapping {
    public static RepositoryDto initRepoByPackageType(String packageType) {
        RepositoryDto repository = new RepositoryDto();
        repository.setArtifactMaxSize(107374182400L);
        JfrogMappingEnum folibLayout = JfrogMappingEnum.getEnumByJfrogName(packageType);
        if(folibLayout==null){
            return null;
        }else {
            repository.setLayout(folibLayout.getLayout());
            repository.setSubLayout(folibLayout.getSubLayout());
        }
        return repository;
    }

    public static String accessConvert(Privilege privilege) {
        if (privilege == ADMIN) {
            return "ARTIFACTS_MANAGE";
        } else if (privilege == DELETE) {
            return "ARTIFACTS_DELETE";
        } else if (privilege == DEPLOY) {
            return "ARTIFACTS_DEPLOY";
        } else if (privilege == ANNOTATE) {
            return "ARTIFACTS_DEPLOY";
        } else if (privilege == READ) {
            return "ARTIFACTS_RESOLVE";
        } else {
            return "";
        }
    }


}
