package com.veadan.folib.users.dto;


import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import com.veadan.folib.users.domain.AccessModelData;
import com.veadan.folib.users.domain.Privileges;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 
 * @author Veadan
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AccessModelDto
        implements AccessModel
{

    private Set<Privileges> apiAuthorities = new LinkedHashSet<>();
    
    private Set<StoragePrivilegesDto> storageAuthorities = new LinkedHashSet<>();


    @Override
    public Set<Privileges> getApiAuthorities()
    {
        return apiAuthorities;
    }

    public Set<StoragePrivilegesDto> getStorageAuthorities()
    {
        return storageAuthorities;
    }

    public Optional<StoragePrivilegesDto> getStorageAuthorities(final String storageId)
    {
        return storageAuthorities.stream().filter(s -> s.getStorageId().equals(storageId)).findFirst();
    }

    @Override
    public Set<Privileges> getPathAuthorities(String url)
    {
        return AccessModelData.getPathAuthorities(url, storageAuthorities);
    }

    @Override
    public Set<Privileges> getPathAuthorities(String storageId, String repositoryId, List<String> paths) {
        return AccessModelData.getPathAuthorities(storageId, repositoryId, paths, storageAuthorities);
    }

}
