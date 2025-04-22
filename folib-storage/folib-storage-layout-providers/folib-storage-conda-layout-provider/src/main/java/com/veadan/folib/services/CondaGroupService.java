package com.veadan.folib.services;


import com.veadan.folib.storage.repository.Repository;

/**
 * @author LingengMa
 * @date 2025/04/22 14:16
 * @Description:
 */

public interface CondaGroupService {
    public void aggregateCondaGroupRepoData(Repository groupRepository, Repository sonRepository);

    public void aggregateCondaGroupRepoData(Repository groupRepository);
}
