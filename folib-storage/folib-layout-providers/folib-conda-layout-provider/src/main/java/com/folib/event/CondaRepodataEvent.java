package com.folib.event;

import com.folib.index.model.RepoData;
import com.folib.index.model.RepoDataEventKind;
import com.folib.index.model.RepoDataPackage;
import com.folib.storage.repository.Repository;
import lombok.Getter;


@Getter
public class CondaRepodataEvent {
    private final RepoDataEventKind type;
    private final Repository repository;
    private final String platformId;
    private final String artifactName;
    private final RepoDataPackage repoDataPackage;
    private final RepoData repoData;

    public CondaRepodataEvent(RepoDataEventKind type, Repository repository, String platformId, String artifactName) {
        this.type = type;
        this.repository = repository;
        this.platformId = platformId;
        this.artifactName = artifactName;
        this.repoDataPackage = null;
        this.repoData = null;
    }

    public CondaRepodataEvent(RepoDataEventKind type, Repository repository, String platformId, String artifactName, RepoDataPackage repoDataPackage) {
        this.type = type;
        this.repository = repository;
        this.platformId = platformId;
        this.artifactName = artifactName;
        this.repoDataPackage = repoDataPackage;
        this.repoData = null;
    }

    public CondaRepodataEvent(RepoDataEventKind type, Repository repository, String platformId, RepoData repoData) {
        this.type = type;
        this.repository = repository;
        this.platformId = platformId;
        this.repoData = repoData;
        this.artifactName = "repodata.json";
        this.repoDataPackage = null;
    }
}

