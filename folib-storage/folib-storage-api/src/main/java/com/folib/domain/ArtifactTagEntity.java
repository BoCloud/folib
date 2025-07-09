package com.folib.domain;

import com.folib.artifact.ArtifactTag;
import com.folib.data.domain.DomainEntity;
import com.folib.db.schema.Vertices;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author veadan
 *
 */
@NodeEntity(Vertices.ARTIFACT_TAG)
public class ArtifactTagEntity
        extends DomainEntity
        implements ArtifactTag
{

    public ArtifactTagEntity()
    {
    }
    
    public ArtifactTagEntity(String name)
    {
        setName(name);
    }

    public void setName(String name)
    {
        setUuid(name);
    }

}
