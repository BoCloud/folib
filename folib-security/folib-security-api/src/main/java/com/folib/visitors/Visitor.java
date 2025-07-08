package com.folib.visitors;

import com.folib.security.Group;
import com.folib.security.exceptions.NotSupportedException;

import java.util.Set;

/**
 * @author veadan
 */
public interface Visitor
{

    void visit(Group group, Set<Group> hierarchy) throws NotSupportedException;

    void endVisit(Group group, Set<Group> hierarchy);

}
