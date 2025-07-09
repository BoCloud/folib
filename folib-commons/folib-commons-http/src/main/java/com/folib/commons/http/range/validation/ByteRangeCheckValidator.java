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
package com.folib.commons.http.range.validation;

import com.folib.commons.http.range.ByteRange;

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
