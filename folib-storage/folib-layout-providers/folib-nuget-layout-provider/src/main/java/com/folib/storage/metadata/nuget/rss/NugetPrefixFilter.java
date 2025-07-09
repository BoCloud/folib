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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.AttributesImpl;
import org.xml.sax.helpers.XMLFilterImpl;

/**
 * Filter that carries the declaration of some namespaces to the root element
 * document
 *
 * @author Veadan
 */
public class NugetPrefixFilter extends XMLFilterImpl
{

    /**
     * @param uriToPrefix
     *            prefix mapping URI
     */
    public NugetPrefixFilter(Map<String, String> uriToPrefix)
    {
        this.uriToPrefix = uriToPrefix;
    }

    /**
     * mapping uri to prefix
     */
    private final Map<String, String> uriToPrefix;

    /**
     * mapping prefix to URI
     */
    private final Map<String, String> prefixToUri = new HashMap<>();

    @Override
    public void startDocument()
        throws SAXException
    {
        super.startDocument();
        for (Entry<String, String> entry : uriToPrefix.entrySet())
        {
            super.startPrefixMapping(entry.getValue(), entry.getKey());
        }
    }

    @Override
    public void endDocument()
        throws SAXException
    {
        for (Entry<String, String> entry : uriToPrefix.entrySet())
        {
            super.endPrefixMapping(entry.getValue());
        }
        super.endDocument();
    }

    @Override
    public void startElement(String uri,
                             String localName,
                             String qName,
                             Attributes atts)
        throws SAXException
    {
        qName = changeNamePrefix(uri, localName, qName);
        AttributesImpl newAttributes = new AttributesImpl();

        for (int i = 0; i < atts.getLength(); i++)
        {
            String aType = atts.getType(i);
            String aqName = atts.getQName(i);
            String aUri = atts.getURI(i);
            String aValue = atts.getValue(i);
            String aLocalName = atts.getLocalName(i);
            if (uriToPrefix.containsKey(aUri))
            {
                aqName = uriToPrefix.get(aUri) + ":" + aLocalName;
            }
            if (!qName.startsWith("xmlns:") && !uriToPrefix.containsKey(aValue))
            {
                newAttributes.addAttribute(aUri, aLocalName, aqName, aType, aValue);
            }
        }
        super.startElement(uri, localName, qName, newAttributes);
    }

    @Override
    public void endElement(String uri,
                           String localName,
                           String qName)
        throws SAXException
    {
        qName = changeNamePrefix(uri, localName, qName);
        super.endElement(uri, localName, qName);
    }

    @Override
    public void startPrefixMapping(String prefix,
                                   String uri)
        throws SAXException
    {
        prefixToUri.put(prefix, uri);
        if (!uriToPrefix.containsKey(uri))
        {
            super.startPrefixMapping(prefix, uri);
        }
    }

    @Override
    public void endPrefixMapping(String prefix)
        throws SAXException
    {
        String uri = prefixToUri.get(prefix);
        if (!uriToPrefix.containsKey(uri))
        {
            super.endPrefixMapping(prefix);
        }
    }

    /**
     * Replaces the prefix of the element name.
     *
     * @param uri
     *            element URI
     * @param localName
     *            name without prefix
     * @param qName
     *            name with prefix
     * @return prefixed name
     */
    private String changeNamePrefix(String uri,
                                    String localName,
                                    String qName)
    {
        if (uri != null && uriToPrefix.containsKey(uri))
        {
            qName = uriToPrefix.get(uri) + ":" + localName;
        }
        return qName;
    }
}
