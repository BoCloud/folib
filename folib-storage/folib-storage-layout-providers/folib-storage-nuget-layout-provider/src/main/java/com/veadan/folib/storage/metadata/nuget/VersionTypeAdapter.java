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

import com.veadan.folib.artifact.coordinates.versioning.SemanticVersion;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * @author Dmitry Sviridov
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
