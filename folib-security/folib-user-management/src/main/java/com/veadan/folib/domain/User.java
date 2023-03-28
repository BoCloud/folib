package com.veadan.folib.domain;

import java.time.LocalDateTime;
import java.util.Set;

import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.data.domain.DomainObject;

import com.fasterxml.jackson.annotation.JsonIgnore;

public interface User extends DomainObject
{

    @Override
    @JsonIgnore
    String getUuid();

    @Override
    default void applyUnfold(Traverser<Vertex> t)
    {

    }

    default String getUsername()
    {
        return getUuid();
    }

    String getEmail();


    default String getUserType(){ return "general";}

    String getPassword();

    Set<SecurityRole> getRoles();

    String getSecurityTokenKey();

    Boolean isEnabled();

    LocalDateTime getLastUpdated();

    String getSourceId();

    String getAvatar();
}
