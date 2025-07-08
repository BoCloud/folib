package com.folib.visitors;

import com.folib.security.Group;
import com.folib.security.exceptions.NotSupportedException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * @author veadan
 */
public class ParentGroupVisitor implements Visitor
{


    @Override
    public void visit(Group group, Set<Group> hierarchy) throws NotSupportedException
    {
        if (group.getParent() != null)
        {
            hierarchy.add(group);
            visit(group.getParent(), hierarchy);
        }
        else
        {
            hierarchy.add(group);
            endVisit(group, hierarchy);
        }
    }

    @Override
    public void endVisit(Group group, Set<Group> hierarchy)
    {
        // Invert the list, so it's top to bottom instead.
        List<Group> list = new ArrayList<>(hierarchy);
        Collections.reverse(list);

        hierarchy.clear();
        hierarchy.addAll(list);
    }

}
