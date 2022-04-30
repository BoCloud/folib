package com.veadan.folib.storage;

import java.util.Map;

import com.veadan.folib.storage.repository.Repository;

public interface Storage
{

    Repository getRepository(String repositoryId);

    String getId();

    String getBasedir();

    Map<String, ? extends Repository> getRepositories();

    boolean containsRepository(String repositoryId);

}
