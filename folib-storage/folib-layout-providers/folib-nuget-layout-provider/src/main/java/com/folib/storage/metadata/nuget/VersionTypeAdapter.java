

package com.folib.storage.metadata.nuget;

import com.folib.artifact.coordinates.versioning.SemanticVersion;
import jakarta.xml.bind.annotation.adapters.XmlAdapter;


/**
 * @author Dmitry Veadan
 */
public class VersionTypeAdapter extends XmlAdapter<String, SemanticVersion>
{

    @Override
    public String marshal(SemanticVersion version)
    {
        if (version == null)
        {
            return null;
        }
        else
        {
            return version.toString();
        }
    }

    @Override
    public SemanticVersion unmarshal(String string)
    {
        if (string == null)
        {
            return null;
        }
        else
        {
            return SemanticVersion.parse(string);
        }
    }

}
