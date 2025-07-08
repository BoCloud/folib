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

package com.folib.storage.metadata.nuget;

import jakarta.xml.bind.JAXBException;

import java.io.OutputStream;


/**
 * The interface of an object that can write its XML representation to a stream
 *
 * @author sviridov
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
