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

/**
 *
 * @author sviridov
 */
public enum MicrosoftTypes
{

    Int32("Edm.Int32"),
    Int64("Edm.Int64"),
    Double("Edm.Double"),
    Boolean("Edm.Boolean"),
    String("Edm.String"),
    DateTime("Edm.DateTime");

    public static MicrosoftTypes parse(String string)
    {
        for (MicrosoftTypes mt : values())
        {
            if (mt.toString().equals(string))
            {
                return mt;
            }
        }
        return null;
    }

    private MicrosoftTypes(String typeName)
    {
        this.typeName = typeName;
    }

    @Override
    public String toString()
    {
        return typeName;
    }

    private final String typeName;
}
