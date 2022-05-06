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

package com.veadan.folib.storage.metadata.nuget;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Date;

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;

/**
 * NuGet interface package
 *
 * @author Unlocker
 */
public interface Nupkg extends Serializable
{
    /**
     * Default extension
     */
    String DEFAULT_EXTENSION = ".nupkg";    

    /**
     * @return package file name
     */
    String getFileName();

    /**
     * @return HASH package file
     */
    String getHash();

    /**
     * @return package specification file
     * @throws NugetFormatException
     *             read package specification
     */
    Nuspec getNuspec()
        throws NugetFormatException;

    /**
     * @return package size
     */
    Long getSize();

    /**
     * @return stream with packet data
     * @throws IOException
     *             data reading error
     */
    InputStream getStream()
        throws IOException;

    /**
     * @return package update date
     */
    Date getUpdated();

    /**
     * @return package identifier
     */
    String getId();

    /**
     * @return version of the package
     */
    SemanticVersion getVersion();
}
