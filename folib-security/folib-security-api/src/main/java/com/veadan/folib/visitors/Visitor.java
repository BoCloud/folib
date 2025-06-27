package com.veadan.folib.visitors;

import com.veadan.folib.security.Group;
import com.veadan.folib.security.exceptions.NotSupportedException;

import java.util.Set;

/**
 * @author veadan
 */
public interface Visitor
{

    void visit(Group group, Set<Group> hierarchy) throws NotSupportedException;

    void endVisit(Group group, Set<Group> hierarchy);

}
