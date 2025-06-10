package com.veadan.folib.commons.http.range.validation;

import com.veadan.folib.commons.http.range.ByteRange;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

/**
 * @author Pablo Tirado
 */
public class ByteRangeCheckValidator
        implements ConstraintValidator<ByteRangeCheck, ByteRange>
{

    @Override
    public void initialize(ByteRangeCheck byteRangeCheck)
    {
        // Nothing here
    }

    @Override
    public boolean isValid(ByteRange byteRange,
                           ConstraintValidatorContext constraintValidatorContext)
    {
        Long start = byteRange.getOffset();
        Long end = byteRange.getLimit();

        // Valid for cases like bytes=500-, where offset is 500 and limit is internally null.
        if (end == null)
        {
            return true;
        }

        // Valid for cases like bytes=-500, where offset is internally zero, and limit is -500.
        if (end < 0)
        {
            return start == 0;
        }

        // Rest of cases, like bytes=0-0,100-200, etc.
        return end >= start;

    }
}
