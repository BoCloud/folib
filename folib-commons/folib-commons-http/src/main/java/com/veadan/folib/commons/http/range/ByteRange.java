package com.veadan.folib.commons.http.range;

import com.veadan.folib.commons.http.range.validation.ByteRangeCheck;

import javax.validation.constraints.Min;

/**
 * @author veadan
 * @author Pablo Tirado
 */
@ByteRangeCheck(message = "Range limit must be greater than or equal to offset")
public class ByteRange
{

    @Min(value = 0, message = "Range offset must be greater than or equal to zero")
    private Long offset;

    private Long limit;

    @Min(value = 0, message = "Range length must be greater than or equal to zero")
    private long totalLength = 0L;

    public ByteRange()
    {
    }

    public ByteRange(Long offset)
    {
        this.offset = offset;
    }

    public ByteRange(Long offset,
                     Long limit)
    {
        this.offset = offset;
        this.limit = limit;
    }

    public Long getOffset()
    {
        return offset;
    }

    public void setOffset(Long offset)
    {
        this.offset = offset;
    }

    public Long getLimit()
    {
        return limit;
    }

    public void setLimit(Long limit)
    {
        this.limit = limit;
    }

    public long getTotalLength()
    {
        return totalLength;
    }

    public void setTotalLength(long totalLength)
    {
        this.totalLength = totalLength;
    }

    @Override
    public String toString()
    {
        final String prefix = "bytes=";

        if (offset == 0 && limit != null && limit < 0)
        {
            if (totalLength == 0)
            {
                return prefix + limit;
            }
            else
            {
                return prefix + (totalLength + limit - 1) + "-" + (totalLength - 1) + "/" + totalLength;
            }
        }
        else if (offset > 0 && limit == null)
        {
            return prefix + (totalLength > 0 ? "-" + totalLength : offset + "-");
        }
        else
        {
            String limitStr = limit != null && limit >= 0 ? "-" + limit : "";
            String totalLengthStr = totalLength > 0 ? "/" + totalLength : "";
            return prefix + offset + limitStr + totalLengthStr;
        }
    }
}
