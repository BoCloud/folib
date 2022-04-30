/*
 * Copyright 2019 Carlspring Consulting & Development Ltd.
 * Copyright 2014 Dmitry Sviridov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.veadan.folib.storage.metadata.nuget.rss;

import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.veadan.folib.storage.metadata.nuget.Author;
import com.veadan.folib.storage.metadata.nuget.NugetFormatException;
import com.veadan.folib.storage.metadata.nuget.Nupkg;
import com.veadan.folib.storage.metadata.nuget.Nuspec;

/**
 *
 * @author Unlocker
 */
@XmlRootElement(name = "entry", namespace = PackageFeed.ATOM_XML_NAMESPACE)
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(propOrder = { "id", "title", "summary", "updated", "author", "links",
                       "category", "content", "properties" })
public class PackageEntry
{

    /**
     * Reads a PackageEntry class from a stream with XML
     *
     * @param inputStream
     *            stream with XML
     * @return recognized PackageEntry instance
     * @throws JAXBException
     *             conversion error
     */
    public static PackageEntry parse(InputStream inputStream)
        throws JAXBException
    {
        JAXBContext context = JAXBContext.newInstance(PackageEntry.class);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        return (PackageEntry) unmarshaller.unmarshal(inputStream);
    }

    /**
     * Attachment title
     */
    @XmlElement(name = "title", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    private Title title;

    /**
     * Description of attachment
     */
    @XmlElement(name = "summary", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    private Title summary = new Title();

    /**
     * Update Date
     */
    @XmlElement(name = "updated", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    private Date updated;

    /**
     * Package Author
     */
    @XmlElement(name = "author", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    private Author author;

    /**
     * Package properties
     */
    @XmlElement(name = "properties", namespace = "http://schemas.microsoft.com/ado/2007/08/dataservices/metadata")
    private EntryProperties properties;

    @XmlElement(name = "link", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    private List<Link> links;

    /**
     * Category RSS Attachments
     */
    private AtomElement category;

    /**
     * A pointer to the contents (archive) package
     */
    private AtomElement content;

    /**
     * Package specification file
     */
    private Nuspec nuspecFile;

    /**
     * Default constructor
     */
    public PackageEntry()
    {
    }

    /**
     * @param nupkgFile
     *            package file
     * @throws NoSuchAlgorithmException
     *             Hash Counting Libraries Not Installed
     * @throws IOException
     *             error reading package file
     * @throws NugetFormatException
     *             incorrect package specification
     */
    public PackageEntry(Nupkg nupkgFile)
        throws NoSuchAlgorithmException,
               IOException,
               NugetFormatException
    {
        this(nupkgFile.getNuspec(), nupkgFile.getHash(), nupkgFile.getSize(), nupkgFile.getUpdated());
    }

    /**
     * @param nuspec
     *            Package Specification
     * @param packageHash
     *            HASH package code
     * @param packageSize
     *            package size
     * @param updateDate
     *            package update date
     */
    public PackageEntry(Nuspec nuspec,
                        String packageHash,
                        Long packageSize,
                        Date updateDate)
    {
        this.nuspecFile = nuspec;
        this.title = new Title(nuspec.getId());
        getProperties().setNuspec(nuspec);
        this.updated = updateDate;
        this.author = new Author(nuspec.getAuthors());
        PackageEntry.this.getLinks()
                         .add(new Link("edit-media", "Package",
                                 "Packages" + getCombineIdAndVersion() + "/$value"));
        PackageEntry.this.getLinks()
                         .add(new Link("edit", "Package",
                                 "Packages" + getCombineIdAndVersion()));
        this.getProperties().setPackageHash(packageHash.toString());
        this.getProperties().setPackageSize(packageSize);
        this.getProperties().setPublished(updateDate);
    }

    private String getCombineIdAndVersion()
    {
        return "(Id='" + getTitle() + "',Version='"
                + getProperties().getVersion().toString() + "')";
    }

    /**
     * @return attachment id
     */
    @XmlElement(name = "id", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    public String getId()
    {
        return getRootUri() + "nuget/Packages(Id='" + getTitle() + "',Version='"
                + getProperties().getVersion().toString() + "')";
    }

    /**
     * @return string value of root repository URI
     */
    protected String getRootUri()
    {
        return null;
    }

    /**
     * @return attachment header (package id)
     */
    public String getTitle()
    {
        return title.value;
    }

    /**
     * @param title
     *            attachment title (package ID)
     */
    public void setTitle(String title)
    {
        this.title = new Title(title);
    }

    public String getSummary()
    {
        return summary.value;
    }

    public void setSummary(String summary)
    {
        this.summary = new Title(summary);
    }

    public Date getUpdated()
    {
        return updated;
    }

    public void setUpdated(Date updated)
    {
        this.updated = updated;
    }

    public Author getAuthor()
    {
        return author;
    }

    public void setAuthor(Author author)
    {
        this.author = author;
    }

    public final EntryProperties getProperties()
    {
        if (properties == null)
        {
            properties = new EntryProperties();
        }
        return properties;
    }

    public List<Link> getLinks()
    {
        if (links == null)
        {
            links = new ArrayList<>();
        }
        return links;
    }

    /**
     * @return category RSS attachments
     */
    @XmlElement(name = "category", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    public AtomElement getCategory()
    {
        if (category == null)
        {
            this.category = new AtomElement();
            category.setTerm("NuGet.Server.DataServices.Package");
            category.setScheme("http://schemas.microsoft.com/ado/2007/08/dataservices/scheme");
        }
        return category;
    }

    /**
     * @param category
     *            category RSS attachments
     */
    protected void setCategory(AtomElement category)
    {
        this.category = category;
    }

    /**
     * @return pointer to the contents (archive) package
     */
    @XmlElement(name = "content", namespace = PackageFeed.ATOM_XML_NAMESPACE)
    public AtomElement getContent()
    {
        if (content == null)
        {
            this.content = new AtomElement();
            content.setType("application/zip");
            content.setSrc(getRootUri() + "download/" + title.value + "/"
                    + nuspecFile.getVersion());
        }
        return content;
    }

    /**
     * @param content
     *            pointer to the contents (archive) package
     */
    protected void setContent(AtomElement content)
    {
        this.content = content;

    }

    /**
     * @param packageSourceUrl
     *            URL where the package is located
     */
    public void setContent(String packageSourceUrl)
    {
        AtomElement newContent = new AtomElement();
        newContent.setType("application/zip");
        newContent.setSrc(packageSourceUrl);
        setContent(newContent);
    }

}
