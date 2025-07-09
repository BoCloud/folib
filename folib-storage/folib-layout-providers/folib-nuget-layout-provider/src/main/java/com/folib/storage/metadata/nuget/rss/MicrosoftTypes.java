

package com.folib.storage.metadata.nuget.rss;

/**
 *
 * @author Veadan
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
