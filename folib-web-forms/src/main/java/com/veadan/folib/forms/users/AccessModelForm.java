package com.veadan.folib.forms.users;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Przemyslaw Fusik
 */
public class AccessModelForm
{

    private List<String> apiAccess = new ArrayList<>();
    
    @Valid
    private List<RepositoryAccessModelForm> repositoriesAccess = new ArrayList<>();

    public List<RepositoryAccessModelForm> getRepositoriesAccess()
    {
        return repositoriesAccess;
    }

    public void setRepositoriesAccess(final List<RepositoryAccessModelForm> repositoriesAccess)
    {
        this.repositoriesAccess = repositoriesAccess;
    }

    public void addRepositoryAccess(RepositoryAccessModelForm repositoryAccess)
    {
        if (repositoriesAccess == null)
        {
            repositoriesAccess = new ArrayList<>();
        }
        repositoriesAccess.add(repositoryAccess);
    }

    public List<String> getApiAccess()
    {
        return apiAccess;
    }

    public void setApiAccess(List<String> apiAccess)
    {
        this.apiAccess = apiAccess;
    }
    
    public void addApiAccess(String privilege)
    {
        if (apiAccess == null)
        {
            apiAccess = new ArrayList<>();
        }

        apiAccess.add(privilege);
    }
    
}
