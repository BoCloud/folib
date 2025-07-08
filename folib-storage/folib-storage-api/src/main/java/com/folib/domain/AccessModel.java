package com.folib.domain;

import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 */
public class AccessModel {

    private List<String> apiAccess = new ArrayList<>();

    private List<RepositoryAccessModel> repositoriesAccess = new ArrayList<>();

    public List<RepositoryAccessModel> getRepositoriesAccess() {
        return repositoriesAccess;
    }

    public void setRepositoriesAccess(final List<RepositoryAccessModel> repositoriesAccess) {
        this.repositoriesAccess = repositoriesAccess;
    }

    public void addRepositoryAccess(RepositoryAccessModel repositoryAccessModel) {
        if (repositoriesAccess == null) {
            repositoriesAccess = new ArrayList<>();
        }
        repositoriesAccess.add(repositoryAccessModel);
    }

    public List<String> getApiAccess() {
        return apiAccess;
    }

    public void setApiAccess(List<String> apiAccess) {
        this.apiAccess = apiAccess;
    }

    public void addApiAccess(String privilege) {
        if (apiAccess == null) {
            apiAccess = new ArrayList<>();
        }

        apiAccess.add(privilege);
    }

}
