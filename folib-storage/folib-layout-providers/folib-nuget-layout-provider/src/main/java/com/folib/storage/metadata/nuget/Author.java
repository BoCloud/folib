

package com.folib.storage.metadata.nuget;



import com.folib.storage.metadata.nuget.rss.PackageFeed;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 *
 * @author Unlocker
 */
@XmlRootElement(name = "author", namespace = PackageFeed.ATOM_XML_NAMESPACE)
@XmlAccessorType(XmlAccessType.NONE)
public class Author
{

    @XmlElement(name = "name", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    private String name;

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public Author()
    {
    }

    public Author(String name)
    {
        this.name = name;
    }
}
