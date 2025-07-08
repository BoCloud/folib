package com.folib.artifact;

import com.folib.data.domain.DomainObject;

public interface ArtifactTag extends DomainObject
{

    String LAST_VERSION = "last-version";
    String RELEASE = "release";

    default String getName()
    {
        return getUuid();
    }

}
