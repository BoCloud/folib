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


package com.folib.storage.metadata.nuget;

import java.util.Arrays;
import java.util.HashSet;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.XMLFilterImpl;

/**
 * @author Veadan
 */
public class NugetNamespaceFilter extends XMLFilterImpl
{

    /**
     * List of URIs to replace
     */
    private final HashSet<String> sourceUris = new HashSet<>();

    /**
     * URI to replace
     */
    private final String targetUri;

    /**
     * Constructor using default namespaces
     */
    public NugetNamespaceFilter()
    {
        this(new String[] { Nuspec.NUSPEC_XML_NAMESPACE_2010,
                            Nuspec.NUSPEC_XML_NAMESPACE_EMPTY,
                            Nuspec.NUSPEC_XML_NAMESPACE_2012,
                            Nuspec.NUSPEC_XML_NAMESPACE_2013,
                            Nuspec.NUSPEC_XML_NAMESPACE_2016,
                            Nuspec.NUSPEC_XML_NAMESPACE_2017,
                            Nuspec.NUSPEC_XML_NAMESPACE_2013_01 },
                Nuspec.NUSPEC_XML_NAMESPACE_2011);
    }

    /**
     * @param sourceUris
     *            list of URIs to replace
     * @param targetUri
     *            URI to replace
     */
    public NugetNamespaceFilter(String[] sourceUris,
                                String targetUri)
    {
        this.sourceUris.addAll(Arrays.asList(sourceUris));
        this.targetUri = targetUri;
    }

    @Override
    public void endElement(String uriInput,
                           String localName,
                           String qName)
        throws SAXException
    {
        String uri = uriInput;
        if (sourceUris.contains(uri))
        {
            uri = targetUri;
        }
        super.endElement(uri, localName, qName);
    }

    @Override
    public void startElement(String uriInput,
                             String localName,
                             String qName,
                             Attributes atts)
        throws SAXException
    {
        String uri = uriInput;
        if (sourceUris.contains(uri))
        {
            uri = targetUri;
        }
        super.startElement(uri, localName, qName, atts);
    }
}
