package com.folib.domain;

import java.io.Serializable;
import java.util.Set;

public interface ArtifactArchiveListing extends Serializable
{

    Set<String> getFilenames();

    void setFilenames(Set<String> filenames);

}
