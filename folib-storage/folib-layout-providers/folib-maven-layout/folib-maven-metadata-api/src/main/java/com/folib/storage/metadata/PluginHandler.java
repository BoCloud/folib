package com.folib.storage.metadata;

import java.util.HashMap;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Created by dinesh on 1/4/18.
 */
public class PluginHandler
        extends DefaultHandler
{

    boolean goalPrefix;
    boolean pluginName;

    String strGoalPrefix, strPluginName;
    HashMap<String, String> pluginMap;

    public HashMap<String, String> getPluginMap()
    {
        return pluginMap;
    }

    @Override
    public void startElement(String uri,
                             String localName,
                             String qName,
                             Attributes attributes)
            throws SAXException
    {

        if (qName.equalsIgnoreCase("plugin"))
        {
            pluginMap = new HashMap<>();
        }
        else if (!goalPrefix && qName.equalsIgnoreCase("goalPrefix"))
        {
            goalPrefix = true;
        }
        else if (!pluginName && qName.equalsIgnoreCase("name"))
        {
            pluginName = true;
        }

    }

    @Override
    public void endElement(String uri,
                           String localName,
                           String qName)
            throws SAXException
    {
        if (qName.equalsIgnoreCase("plugin"))
        {
            pluginMap.put("goalPrefix", strGoalPrefix);
            pluginMap.put("name", strPluginName);

        }
    }

    @Override
    public void characters(char ch[],
                           int start,
                           int length)
            throws SAXException
    {
        if (goalPrefix && null == strGoalPrefix)
        {
            strGoalPrefix = new String(ch, start, length);
        }
        else if (pluginName && null == strPluginName)
        {
            strPluginName = new String(ch, start, length);
        }
    }
}
