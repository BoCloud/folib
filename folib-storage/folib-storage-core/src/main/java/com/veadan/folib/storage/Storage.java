package com.veadan.folib.storage;

import java.util.Map;
import java.util.Set;

import com.veadan.folib.storage.repository.Repository;

public interface Storage
{

    Repository getRepository(String repositoryId);

    String getId();

    String getBasedir();

    Set<String> getUsers();

    Map<String, ? extends Repository> getRepositories();

    boolean containsRepository(String repositoryId);

}
