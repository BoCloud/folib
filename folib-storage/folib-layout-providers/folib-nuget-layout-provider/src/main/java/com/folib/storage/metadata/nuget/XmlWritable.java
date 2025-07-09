

package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.JAXBException;

import java.io.OutputStream;


/**
 * The interface of an object that can write its XML representation to a stream
 *
 * @author Veadan
 */
public interface XmlWritable
{

    /**
     * Writes an XML representation of the object to the stream
     *
     * @param outputStream
     *            stream for recording
     * @throws JAXBException
     *             error converting object to XML
     */
    public void writeXml(OutputStream outputStream)
        throws JAXBException;
}
