package com.veadan.folib.security;

import com.veadan.folib.security.exceptions.NotSupportedException;

/**
 * @author veadan
 */
public interface Group
{

    String getName();

    String getDescription();

    Group getParent() throws NotSupportedException;

}
