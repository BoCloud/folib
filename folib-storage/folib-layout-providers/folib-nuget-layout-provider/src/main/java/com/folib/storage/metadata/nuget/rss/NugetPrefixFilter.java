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
 * @author sviridov
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
