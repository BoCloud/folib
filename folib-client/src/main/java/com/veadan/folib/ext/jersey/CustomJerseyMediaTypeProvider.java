package com.veadan.folib.ext.jersey;

import javax.ws.rs.core.MediaType;
import java.text.ParseException;
import java.util.Map;
import java.util.regex.Pattern;

import org.glassfish.jersey.internal.LocalizationMessages;
import org.glassfish.jersey.message.internal.HttpHeaderReader;
import org.glassfish.jersey.message.internal.MediaTypeProvider;
import org.springframework.util.Assert;

/**
 * @author Przemyslaw Fusik
 */
public class CustomJerseyMediaTypeProvider
        extends MediaTypeProvider
{

    /**
     * Remote repository maven.oracle.com responses with incomplete content-type header like 'Application/jar; charset=' or 'Application/pom; charset='
     */
    private static final Pattern INCOMPLETE_CONTENT_TYPE_HEADER_PATTERN = Pattern.compile(
            "^Application/[a-z/+/-]+; charset=$");

    private static MediaType valueOf(String header)
            throws ParseException
    {
        HttpHeaderReader reader = HttpHeaderReader.newInstance(header);

        // Skip any white space
        reader.hasNext();

        // Get the type
        final String type = reader.nextToken().toString();
        reader.nextSeparator('/');
        // Get the subtype
        final String subType = reader.nextToken().toString();

        Map<String, String> params = null;

        if (reader.hasNext())
        {
            try
            {
                params = HttpHeaderReader.readParameters(reader);
            }
            catch (ParseException ex)
            {
                if (!(LocalizationMessages.HTTP_HEADER_END_OF_HEADER().equals(ex.getMessage()) &&
                      INCOMPLETE_CONTENT_TYPE_HEADER_PATTERN.matcher(header).matches()))
                {
                    throw ex;
                }
            }

        }

        return new MediaType(type, subType, params);
    }

    @Override
    public MediaType fromString(String header)
    {

        Assert.notNull(header, "header should not be null");

        try
        {
            return valueOf(header);
        }
        catch (ParseException ex)
        {
            throw new IllegalArgumentException("Error parsing media type '" + header + "'", ex);
        }
    }
}
