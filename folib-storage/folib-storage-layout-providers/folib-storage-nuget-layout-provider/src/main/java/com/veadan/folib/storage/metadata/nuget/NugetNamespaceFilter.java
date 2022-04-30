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

package com.veadan.folib.storage.metadata.nuget;

import java.util.Arrays;
import java.util.HashSet;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.XMLFilterImpl;

/**
 * @author sviridov
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
