

package com.folib.storage.metadata.nuget.rss;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;

import java.util.Objects;


/**
 *
 * @author Veadan
 */
@XmlRootElement(name = "link", namespace = PackageFeed.ATOM_XML_NAMESPACE)
@XmlAccessorType(XmlAccessType.NONE)
class Link
{

    @XmlAttribute(name = "rel")
    private String rel;

    @XmlAttribute(name = "title")
    private String title;

    @XmlAttribute(name = "href")
    private String href;

    public Link()
    {
    }

    public Link(String rel,
                String title,
                String href)
    {
        this.rel = rel;
        this.title = title;
        this.href = href;
    }

    public String getHref()
    {
        return href;
    }

    public void setHref(String href)
    {
        this.href = href;
    }

    public String getRel()
    {
        return rel;
    }

    public void setRel(String rel)
    {
        this.rel = rel;
    }

    public String getTitle()
    {
        return title;
    }

    public void setTitle(String title)
    {
        this.title = title;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }
        if (getClass() != obj.getClass())
        {
            return false;
        }
        final Link other = (Link) obj;
        if (!Objects.equals(this.rel, other.rel))
        {
            return false;
        }
        if (!Objects.equals(this.title, other.title))
        {
            return false;
        }
        if (!Objects.equals(this.href, other.href))
        {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode()
    {
        int hash = 5;
        hash = 97 * hash + Objects.hashCode(this.rel);
        hash = 97 * hash + Objects.hashCode(this.title);
        hash = 97 * hash + Objects.hashCode(this.href);
        return hash;
    }
}
