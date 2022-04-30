package com.veadan.folib.domain;

import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.data.domain.DomainEntity;
import com.veadan.folib.db.schema.Vertices;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author Sergey Bespalov
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
