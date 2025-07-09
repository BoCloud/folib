

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
