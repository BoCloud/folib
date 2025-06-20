package com.veadan.folib.dto.users;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * @author veadan
 */
public class AccessModelDto
{

    private List<String> apiAccess = new ArrayList<>();
    
    @Valid
    private List<RepositoryAccessModelDto> repositoriesAccess = new ArrayList<>();

    public List<RepositoryAccessModelDto> getRepositoriesAccess()
    {
        return repositoriesAccess;
    }

    public void setRepositoriesAccess(final List<RepositoryAccessModelDto> repositoriesAccess)
    {
        this.repositoriesAccess = repositoriesAccess;
    }

    public void addRepositoryAccess(RepositoryAccessModelDto repositoryAccess)
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
