package com.folib.security;

import com.folib.security.exceptions.NotSupportedException;

/**
 * @author veadan
 */
public interface Group
{

    String getName();

    String getDescription();

    Group getParent() throws NotSupportedException;

}
