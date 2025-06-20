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

package com.veadan.folib.storage.metadata.nuget.rss;


import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;

/**
 *
 * @author sviridov
 */
@XmlAccessorType(XmlAccessType.NONE)
public class AtomElement
{

    @XmlAttribute
    private String term;

    @XmlAttribute
    private String type;

    @XmlAttribute
    private String scheme;

    @XmlAttribute
    private String src;

    public AtomElement()
    {
    }

    public String getScheme()
    {
        return scheme;
    }

    public void setScheme(String scheme)
    {
        this.scheme = scheme;
    }

    /**
     * @return URL where you can get the stream with the package
     */
    public String getSrc()
    {
        return src;
    }

    public void setSrc(String src)
    {
        this.src = src;
    }

    public String getTerm()
    {
        return term;
    }

    public void setTerm(String term)
    {
        this.term = term;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }
}
