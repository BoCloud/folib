package com.veadan.folib.visitors;

import com.veadan.folib.security.Group;
import com.veadan.folib.security.exceptions.NotSupportedException;

import java.util.LinkedHashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;

/**
 * @author mtodorov
 */
public class ParentGroupVisitorTest
{

    @Test
    public void testVisitor()
    {
        Group group1 = createGroup("employees", "Employees", null);
        Group group2 = createGroup("developers", "Developers", group1);
        Group group3 = createGroup("java-developers-uk", "Java Developers UK", group2);

        Set<Group> nestedGroups = new LinkedHashSet<>();
        ParentGroupVisitor visitor = new ParentGroupVisitor();

        try
        {
            visitor.visit(group3, nestedGroups);
            for (Group group : nestedGroups)
            {
                System.out.println(group.getName());
            }
        }
        catch (NotSupportedException e)
        {
            e.printStackTrace();
        }

    }

    private Group createGroup(final String name, final String description, final Group parent)
    {
        //noinspection UnnecessaryLocalVariable
        Group group = new Group()
        {

            @Override
            public String getName()
            {
                return name;
            }

            @Override
            public String getDescription()
            {
                // We don't care.
                return description;
            }

            @Override
            public Group getParent()
            {
                return parent;
            }

        };

        return group;
    }

}
