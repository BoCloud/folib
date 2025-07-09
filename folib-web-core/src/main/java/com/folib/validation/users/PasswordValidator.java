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
package com.folib.validation.users;



import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang.StringUtils;
import org.slf4j.helpers.MessageFormatter;

public class PasswordValidator
        implements ConstraintValidator<Password, String>
{

    private Password constraint;

    @Override
    public void initialize(Password constraint)
    {
        this.constraint = constraint;
    }

    @Override
    public boolean isValid(String password,
                           ConstraintValidatorContext context)
    {
        if (StringUtils.isBlank(password))
        {
            if (constraint.allowNull())
            {
                return true;
            }
            else
            {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate(constraint.message())
                       .addConstraintViolation();

                return false;
            }
        }

        boolean isValid = true;
        int length = password.length();
        if (length < constraint.min())
        {
            isValid = false;
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(
                    format(constraint.minMessage(), constraint.min())
            ).addConstraintViolation();
        }
        else if (length > constraint.max())
        {
            isValid = false;
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(
                    format(constraint.maxMessage(), constraint.max())
            ).addConstraintViolation();
        }

        return isValid;
    }

    public static String format(String msg, Object... objs) {
        return MessageFormatter.arrayFormat(msg, objs).getMessage();
    }

}
