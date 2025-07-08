/*
 * Copyright 2019 Veadan Team & Development Ltd.
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

package com.folib.storage.metadata.nuget.rss;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.JAXBException;
import jakarta.xml.bind.Marshaller;
import jakarta.xml.bind.Unmarshaller;
import jakarta.xml.bind.annotation.*;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import com.folib.storage.metadata.nuget.XmlWritable;

/**
 *
 * @author Unlocker
 */
@XmlRootElement(name = "feed", namespace = PackageFeed.ATOM_XML_NAMESPACE)
@XmlAccessorType(XmlAccessType.NONE)
@XmlType(propOrder = { "title", "id", "updated", "link", "entries" })
public class PackageFeed implements XmlWritable
{

    public static final String ATOM_XML_NAMESPACE = "http://www.w3.org/2005/Atom";

    public static PackageFeed parse(InputStream inputStream)
        throws JAXBException
    {
        JAXBContext context = JAXBContext.newInstance(PackageFeed.class);
        Unmarshaller unmarshaller = context.createUnmarshaller();
        return (PackageFeed) unmarshaller.unmarshal(inputStream);
    }

    /**
     * Name of RSS feed
     */
    @XmlElement(name = "title", namespace = ATOM_XML_NAMESPACE)
    private Title title = new Title("Packages");

    /**
     * Address storage packages
     */
    @XmlElement(name = "id", namespace = ATOM_XML_NAMESPACE)
    private String id;

    /**
     * Last modified date in storage
     */
    @XmlElement(name = "updated", type = Date.class, namespace = ATOM_XML_NAMESPACE)
    private Date updated;

    /**
     * Link to packages
     */
    @XmlElement(name = "link", namespace = ATOM_XML_NAMESPACE)
    private Link link = new Link("self", "Packages", "Packages");

    /**
     * Packet descriptions
     */
    @XmlElement(name = "entry", namespace = ATOM_XML_NAMESPACE)
    private List<PackageEntry> entries;

    /**
     * @return pact description
     */
    public List<PackageEntry> getEntries()
    {
        if (entries == null)
        {
            entries = new ArrayList<>();
        }
        return entries;
    }

    /**
     * @param entries
     *            for pact descriptions
     */
    public void setEntries(List<PackageEntry> entries)
    {
        this.entries = entries;
    }

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public String getTitle()
    {
        return title != null ? title.value : null;
    }

    public void setTitle(String title)
    {
        this.title = new Title(title);
    }

    public Date getUpdated()
    {
        return updated;
    }

    public void setUpdated(Date updated)
    {
        this.updated = updated;
    }

    /**
     * @return link to packages
     */
    public String getLink()
    {
        return link.getHref();
    }

    /**
     * @param link
     *            link to packages
     */
    public void setLink(String link)
    {
        this.link = new Link("self", "Packages", link);
    }

    /**
     * @return XML object representation
     * @throws JAXBException
     *             XML conversion error
     */
    public String getXml()
        throws JAXBException
    {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        writeXml(byteArrayOutputStream);
        return new String(byteArrayOutputStream.toByteArray());
    }

    /**
     * Writes a compelling class as an XML document to a stream.
     *
     * @param outputStream
     *            stream for recording
     * @throws JAXBException
     *             XML conversion error
     */
    @Override
    public void writeXml(OutputStream outputStream)
        throws JAXBException
    {
        // Initial Serialization
        JAXBContext context = JAXBContext.newInstance(this.getClass());
        Marshaller marshaller = context.createMarshaller();
        Map<String, String> uriToPrefix = new HashMap<>();
        uriToPrefix.put("http://www.w3.org/2005/Atom", "atom");
        uriToPrefix.put("http://schemas.microsoft.com/ado/2007/08/dataservices/metadata", "m");
        uriToPrefix.put("http://schemas.microsoft.com/ado/2007/08/dataservices/scheme", "ds");
        uriToPrefix.put("http://schemas.microsoft.com/ado/2007/08/dataservices", "d");
        NugetPrefixFilter filter = new NugetPrefixFilter(uriToPrefix);
        filter.setContentHandler(new XMLSerializer(outputStream, new OutputFormat()));
        marshaller.marshal(this, filter);
    }

}
