package com.folib.job.cron.domain;

import javax.annotation.concurrent.Immutable;
import java.util.Collections;
import java.util.List;

import com.google.common.collect.ImmutableList;
import jakarta.xml.bind.annotation.XmlElement;

/**
 * @author veadan
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
