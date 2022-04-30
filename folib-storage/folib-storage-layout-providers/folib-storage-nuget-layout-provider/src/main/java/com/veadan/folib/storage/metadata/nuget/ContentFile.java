
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

@XmlRootElement(name = "files", namespace = Nuspec.NUSPEC_XML_NAMESPACE_2011)
@XmlAccessorType(XmlAccessType.NONE)
public class ContentFile implements Serializable
{

    @XmlAttribute(name = "include")
    private String include;

    @XmlAttribute(name = "exclude")
    private String exclude;

    @XmlAttribute(name = "buildAction")
    private String buildAction;

    @XmlAttribute(name = "copyToOutput")
    private Boolean copyToOutput;

    @XmlAttribute(name = "flatten")
    private Boolean flatten;

    protected String getInclude()
    {
        return include;
    }

    protected void setInclude(String include)
    {
        this.include = include;
    }

    protected String getBuildAction()
    {
        return buildAction;
    }

    protected void setBuildAction(String buildAction)
    {
        this.buildAction = buildAction;
    }

    protected Boolean getCopyToOutput()
    {
        return copyToOutput;
    }

    protected void setCopyToOutput(Boolean copyToOutput)
    {
        this.copyToOutput = copyToOutput;
    }

    protected String getExclude()
    {
        return exclude;
    }

    protected void setExclude(String exclude)
    {
        this.exclude = exclude;
    }

    protected Boolean getFlatten()
    {
        return flatten;
    }

    protected void setFlatten(Boolean flatten)
    {
        this.flatten = flatten;
    }

}
