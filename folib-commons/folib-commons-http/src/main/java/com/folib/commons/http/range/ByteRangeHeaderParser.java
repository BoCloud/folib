package com.folib.commons.http.range;

import com.folib.commons.http.range.validation.ByteRangeValidationException;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import java.util.*;

/**
 * @author veadan
 * @author Pablo Tirado
 */
public class ByteRangeHeaderParser
{

    static final String BYTE_RANGE_NOT_VALID_MESSAGE = "The byte range provided is not valid.";

    private String headerContents;

    private Validator validator;


    public ByteRangeHeaderParser(String headerContents)
    {
        this.headerContents = headerContents;

        initValidationFactory();
    }

    private void initValidationFactory()
    {
        ValidatorFactory validatorFactory = Validation.buildDefaultValidatorFactory();
        validator = validatorFactory.getValidator();
    }

    /**
     * Returns the list of ranges denoted by the "Range:" header.
     *
     * @return
     */
    public List<ByteRange> getRanges()
    {
        List<ByteRange> byteRanges = new ArrayList<>();

        String byteRangesHeader = headerContents.substring(headerContents.lastIndexOf('=') + 1);

        long length = byteRangesHeader.contains("/") && !byteRangesHeader.endsWith("/*") ?
                      Long.parseLong(byteRangesHeader.substring(byteRangesHeader.lastIndexOf('/') + 1)) : 0;

        String[] ranges = byteRangesHeader.split(",");
        String[] rangesWithoutLength = Arrays.stream(ranges)
                                             .map(range -> range.contains("/") ?
                                                           range.substring(0, range.indexOf('/')) :
                                                           range)
                                             .toArray(String[]::new);

        for (String range : rangesWithoutLength)
        {
            ByteRange byteRange = createByteRangeFromPosition(range);
            if (byteRange != null)
            {
                byteRange.setTotalLength(length);

                Set<ConstraintViolation<ByteRange>> violations = validator.validate(byteRange);
                if (!violations.isEmpty())
                {
                    handleByteRangeConstraintViolations(violations, byteRange);
                }

                byteRanges.add(byteRange);
            }
            else
            {
                throw new ByteRangeValidationException(BYTE_RANGE_NOT_VALID_MESSAGE);
            }
        }

        return byteRanges;
    }

    private ByteRange createByteRangeFromPosition(String range)
    {
        ByteRange byteRange = null;

        final String firstNBytesRegex = "^\\d+-?$";
        final String betweenNBytesRegex = "^(\\d+-\\d+)$";
        final String lastNBytesRegex = "^-\\d+$";

        if (range.matches(firstNBytesRegex))
        {
            Long start;
            if (range.endsWith("-"))
            {
                // Example: 1000- ; Read all bytes after 1000.
                start = Long.parseLong(range.substring(0, range.length() - 1));
            }
            else
            {
                // Example: 2000 ; Read all bytes after 2000.
                start = Long.parseLong(range);
            }

            byteRange = new ByteRange(start);
        }
        else if (range.matches(betweenNBytesRegex))
        {
            // Example: 1000-2000 ; Read bytes 1000-2000 (incl.)
            String[] rangeElements = range.split("-");
            Long start = Long.parseLong(rangeElements[0]);
            Long end = Long.parseLong(rangeElements[1]);
            byteRange = new ByteRange(start, end);
        }
        else if (range.matches(lastNBytesRegex))
        {
            // Example: -2000 ; Read the last 2000 bytes.
            Long start = 0L;
            Long end = Long.parseLong(range);
            byteRange = new ByteRange(start, end);
        }

        return byteRange;
    }

    private void handleByteRangeConstraintViolations(Set<ConstraintViolation<ByteRange>> violations,
                                                     ByteRange byteRange)
    {
        Optional<String> errorMessageOptional = violations.stream().findFirst().map(ConstraintViolation::getMessage);
        if (errorMessageOptional.isPresent())
        {
            String byteRangeStr =
                    byteRange.getTotalLength() != 0 ? byteRange.toString() + "/" + byteRange.getTotalLength() :
                    byteRange.toString();
            throw new ByteRangeValidationException(byteRangeStr + ": " + errorMessageOptional.get());
        }
    }

}
