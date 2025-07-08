package com.folib.jtwig.extensions;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import org.jtwig.functions.FunctionRequest;
import org.jtwig.functions.JtwigFunction;

/**
 * 
 * @author veadan
 *
 */
public class HumanReadableSizeConversionFunction implements JtwigFunction
{

    @Override
    public String name()
    {
        return "humanReadableSize";
    }

    @Override
    public Collection<String> aliases()
    {
        return Collections.unmodifiableList(Arrays.asList("readableSize"));
    }

    @Override
    public Object execute(FunctionRequest request)
    {
        request.minimumNumberOfArguments(1);
        request.maximumNumberOfArguments(1);
        String byteSize = request.getEnvironment().getValueEnvironment().getStringConverter().convert(request.get(0));
        try
        {
            return getHumanReadableSize(Long.parseLong(byteSize));
        }
        catch (NumberFormatException nfe)
        {
            // Need to return same value of bytes
        }
        return byteSize;
    }

    private String getHumanReadableSize(long bytes)
    {
        int unit = 1000;
        if (bytes < unit)
        {
            return bytes + " B";
        }
        int exp = (int) (Math.log(bytes) / Math.log(unit));
        String pre = "" + "KMGTPE".charAt(exp - 1);
        return String.format("%.1f %sB", bytes / Math.pow(unit, exp), pre);
    }

}
