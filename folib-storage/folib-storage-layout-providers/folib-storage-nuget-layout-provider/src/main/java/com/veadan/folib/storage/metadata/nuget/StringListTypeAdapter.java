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

import javax.xml.bind.annotation.adapters.XmlAdapter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author Unlocker
 */
public class StringListTypeAdapter extends XmlAdapter<String, List<String>>
{

    /**
     * Default splitter pattern
     */
    private String delimeter = "\\s+";

    /**
     * Remove whitespaces
     */
    private boolean trimSpaces = false;

    /**
     * Default constructor
     */
    public StringListTypeAdapter()
    {
    }

    /**
     * @param delimeter
     *            REGEXP delimiter
     * @param trimSpaces
     *            whether to trim spaces
     */
    public StringListTypeAdapter(String delimeter,
                                 boolean trimSpaces)
    {
        this.delimeter = delimeter;
        this.trimSpaces = trimSpaces;
    }

    @Override
    public List<String> unmarshal(String v)
    {
        String pattern = trimSpaces ? "\\s*" + delimeter + "\\s*" : delimeter;
        String[] temp = v.split(pattern);
        List<String> result = new ArrayList<>();

        for (String str : temp)
        {
            String tag = str.trim();
            if (!tag.isEmpty())
            {
                result.add(tag);
            }
        }

        return result;
    }

    @Override
    public String marshal(List<String> v)
        throws Exception
    {
        Iterator<String> iter = v.iterator();
        if (!iter.hasNext())
        {
            return "";
        }

        StringBuilder buffer = new StringBuilder(iter.next());
        while (iter.hasNext())
        {
            buffer.append(delimeter).append(iter.next());
        }

        return buffer.toString();
    }
}
