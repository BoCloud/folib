package com.veadan.folib.cron.domain;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlElement;
import java.util.Collections;
import java.util.List;

import com.google.common.collect.ImmutableList;

/**
 * @author Przemyslaw Fusik
 */
@Immutable
public class GroovyScriptNames
{

    @XmlElement
    private final List<String> list;

    public GroovyScriptNames(final GroovyScriptNamesDto source)
    {
        this.list = immuteList(source.getList());
    }

    private List<String> immuteList(final List<String> source)
    {
        return source != null ? ImmutableList.copyOf(source) : Collections.emptyList();
    }
}
