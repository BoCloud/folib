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

import java.io.Serializable;
import java.util.Objects;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * File reference
 * 
 * @author Unlocker
 */
public class Reference implements Serializable
{

    /**
     * File name
     */
    @XmlAttribute(name = "file")
    private String file;

    /**
     * @param file
     *            new file name
     * @return this instance.
     */
    public Reference setFile(String file)
    {
        this.file = file;
        return this;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj == null)
        {
            return false;
        }
        if (!(obj instanceof Reference))
        {
            return false;
        }
        Reference o = (Reference) obj;
        return Objects.equals(o.file, this.file);
    }

    @Override
    public int hashCode()
    {
        return Objects.hash(this.file);
    }
}
