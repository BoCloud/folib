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
import java.util.EnumSet;

import com.google.common.base.Joiner;
import com.google.common.base.Strings;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * List to frameworks converter
 */
public class AssemblyTargetFrameworkAdapter extends XmlAdapter<String, EnumSet<Framework>>
{

    /**
     * Logger
     */
    private final static Logger logger = LoggerFactory.getLogger(AssemblyTargetFrameworkAdapter.class);

    /**
     * Frameworks delimeter
     */
    private static final String FRAMEWORKS_DELIMETER = ", ";

    @Override
    public String marshal(EnumSet<Framework> frameworks)
        throws Exception
    {
        if (frameworks == null || frameworks.isEmpty())
        {
            return null;
        }

        String result = Joiner.on(FRAMEWORKS_DELIMETER).join(frameworks);
        return result;
    }

    @Override
    public EnumSet<Framework> unmarshal(String farmeworks)
        throws Exception
    {
        if (Strings.isNullOrEmpty(farmeworks))
        {
            return null;
        }

        String[] names = farmeworks.split(FRAMEWORKS_DELIMETER);
        EnumSet<Framework> result = EnumSet.noneOf(Framework.class);
        for (String name : names)
        {
            try
            {
                final Framework framework = Framework.getByFullName(name);

                if (framework != null)
                {
                    result.add(framework);
                }
            }
            catch (Exception e)
            {
                logger.warn("Can not add framework: \"{}\"", name, e);
            }
        }

        if (result.isEmpty())
        {
            return null;
        }

        return result;
    }
}
