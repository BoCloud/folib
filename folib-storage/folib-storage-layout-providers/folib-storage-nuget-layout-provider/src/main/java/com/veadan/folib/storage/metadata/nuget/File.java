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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.Serializable;

@XmlRootElement(name = "file", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class File implements Serializable
{

    @XmlAttribute(name = "src")
    private String src;

    @XmlAttribute(name = "target")
    private String target;

    @XmlAttribute(name = "exclude")
    private String exclude;

    protected String getSrc()
    {
        return src;
    }

    protected void setSrc(String src)
    {
        this.src = src;
    }

    protected String getTarget()
    {
        return target;
    }

    protected void setTarget(String target)
    {
        this.target = target;
    }

    protected String getExclude()
    {
        return exclude;
    }

    protected void setExclude(String exclude)
    {
        this.exclude = exclude;
    }

}
