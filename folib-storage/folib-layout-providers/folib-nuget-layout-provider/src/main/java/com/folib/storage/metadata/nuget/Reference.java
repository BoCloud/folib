

package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.annotation.XmlAttribute;

import java.io.Serializable;
import java.util.Objects;


/**
 * File reference
 * 
 * @author Unlocker
 */
public class Reference implements Serializable
{

    /**
     * File name
     */
    @XmlAttribute(name = "file")
    private String file;

    /**
     * @param file
     *            new file name
     * @return this instance.
     */
    public Reference setFile(String file)
    {
        this.file = file;
        return this;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }
        if (!(obj instanceof Reference))
        {
            return false;
        }
        Reference o = (Reference) obj;
        return Objects.equals(o.file, this.file);
    }

    @Override
    public int hashCode()
    {
        return Objects.hash(this.file);
    }
}
