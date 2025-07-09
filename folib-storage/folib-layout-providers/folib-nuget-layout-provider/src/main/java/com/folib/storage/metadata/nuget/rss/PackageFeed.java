/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
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

//    /**
//     * Packet descriptions
//     */
//    @XmlElement(name = "entry", namespace = ATOM_XML_NAMESPACE)
//    private List<PackageEntry> entries;
//
//    /**
//     * @return pact description
//     */
//    public List<PackageEntry> getEntries()
//    {
//        if (entries == null)
//        {
//            entries = new ArrayList<>();
//        }
//        return entries;
//    }

    /**
     * @param entries
     *            for pact descriptions
     */
//    public void setEntries(List<PackageEntry> entries)
//    {
//        this.entries = entries;
//    }

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
