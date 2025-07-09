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
package com.folib.security.authentication.suppliers;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import java.util.List;

import jakarta.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

/**
 * @author veadan
 */
public class AuthenticationSuppliers
        implements AuthenticationSupplier
{

    private static final Logger logger = LoggerFactory.getLogger(AuthenticationSuppliers.class);

    private final List<AuthenticationSupplier> suppliers;

    public AuthenticationSuppliers(List<AuthenticationSupplier> suppliers)
    {
        this.suppliers = suppliers;
    }

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request)
    {
        Authentication authentication;
        if (suppliers == null || suppliers.isEmpty())
        {
            logger.debug("There was no [{}] provided.", AuthenticationSupplier.class);
            
            return null;
        }

        AuthenticationException lastException = null;
        for (final AuthenticationSupplier supplier : suppliers)
        {
            final String supplierName = supplier.getClass()
                                                .getName();

            if (!supplier.supports(request))
            {
                logger.debug("Supplier {} does not support this request [method: {}] [URI: {}] [ContentType {}]",
                             supplierName, request.getMethod(), request.getRequestURI(), request.getContentType());
                continue;
            }

            logger.debug("Authentication supplier attempt using {}", supplierName);
            try
            {
                authentication = supplier.supply(request);
            }
            catch (AuthenticationException e)
            {
                lastException = e;
                continue;
            }

            if (authentication != null)
            {
                logger.debug("Authentication supplied by {}", supplierName);

                return authentication;
            }
        }
        if (lastException != null)
        {
            throw lastException;
        }
        
        return null;
    }
    
}
