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
