/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.commons.http.range;

import com.folib.commons.http.range.validation.ByteRangeCheck;

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
