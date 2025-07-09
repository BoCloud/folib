

package com.folib.storage.metadata.nuget;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Date;

import com.folib.artifact.coordinates.versioning.SemanticVersion;

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
