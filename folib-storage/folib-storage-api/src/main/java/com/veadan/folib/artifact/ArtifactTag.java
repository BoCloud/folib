package com.veadan.folib.artifact;

import com.veadan.folib.data.domain.DomainObject;

public interface ArtifactTag extends DomainObject
{

    String LAST_VERSION = "last-version";
    String RELEASE = "release";

    default String getName()
    {
        return getUuid();
    }

}
