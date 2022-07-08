package com.veadan.folib.data.domain;

import java.io.Serializable;

import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Vertex;

/**
 * @author xuxinping
 *
 */
public interface DomainObject extends Serializable
{
    default Long getNativeId()
    {
        return null;
    }

    String getUuid();
    
    void applyUnfold(Traverser<Vertex> t);

}
