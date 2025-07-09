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
package com.folib.ldap;

import java.util.Base64;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.crypto.codec.Hex;
import org.springframework.security.crypto.codec.Utf8;
import org.springframework.security.ldap.userdetails.LdapUserDetailsMapper;
import org.springframework.util.StringUtils;

/**
 * This class handles password base64 decoding based on the property folib.authentication.ldap.userPasswordEncoded.
 *
 * <p>
 *  When set to true will handle these possible cases:
 *  {ALG}base64.encode(md5/sha1/bcrypt(mypassword))
 *  base64.encode({ALG}md5/sha1/bcrypt(mypassword))
 * <p>
 *
 * <p>
 *  When set to false will handle the ordinary case:
 *  {ALG}md5/sha1/bcrypt(mypassword)
 * </p>
 *
 * @author veadan
 * @date 19/10/20
 */
public class CustomLdapUserDetailsMapper
        extends LdapUserDetailsMapper
{

    private static final String PREFIX = "{";

    private static final String SUFFIX = "}";

    private static final String EMPTY_STRING = "";

    private static final Logger logger = LoggerFactory.getLogger(CustomLdapUserDetailsMapper.class);

    private boolean isUserPasswordEncoded;

    @Override
    protected String mapPassword(Object passwordValue)
    {
        String passwordValueString = super.mapPassword(passwordValue);

        if (!isUserPasswordEncoded())
        {
            return passwordValueString;
        }

        return decodeBase64EncodedPassword(passwordValueString);
    }

    private String decodeBase64EncodedPassword(String prefixEncodedPasswordString)
    {
        try
        {
            String algorithmUsed = extractId(prefixEncodedPasswordString);
            String extractBase64EncodedHash = prefixEncodedPasswordString;

            if (!StringUtils.isEmpty(algorithmUsed))
            {
                extractBase64EncodedHash = extractEncodedPassword(prefixEncodedPasswordString);

                return PREFIX + algorithmUsed + SUFFIX + decodeBase64EncodedHashWithHex(extractBase64EncodedHash);
            }
            else
            {
                return new String(Base64.getDecoder().decode(Utf8.encode(extractBase64EncodedHash)));
            }
        }
        catch (Exception e)
        {
            logger.warn("Failed to match password after decoding base64encoded hash after algorithm", e);

            return prefixEncodedPasswordString;
        }
    }

    private String decodeBase64EncodedHashWithHex(String base64EncodedHash)
    {
        try
        {
            return new String(Hex.encode(Base64.getDecoder().decode(Utf8.encode(base64EncodedHash))));
        }
        catch (Exception ex)
        {
            logger.warn("decode hash using base64! " + ex.getMessage(), ex);
        }

        return base64EncodedHash;
    }

    private String extractEncodedPassword(String prefixEncodedPassword)
    {
        int start = prefixEncodedPassword.indexOf(SUFFIX);

        return prefixEncodedPassword.substring(start + 1);
    }

    private String extractId(String prefixEncodedPassword)
    {
        int start = prefixEncodedPassword.indexOf(PREFIX);

        if (start != 0)
        {
            return EMPTY_STRING;
        }

        int end = prefixEncodedPassword.indexOf(SUFFIX, start);

        if (end < 0)
        {
            return EMPTY_STRING;
        }

        return prefixEncodedPassword.substring(start + 1, end);
    }


    /**
     * Getting value whether Base64EncodedPassword is enabled or not
     *
     * @return boolean
     */
    public boolean isUserPasswordEncoded()
    {
        return isUserPasswordEncoded;
    }


    /**
     * Setting value whether Base64EncodedPassword is enabled or not
     *
     * @param userPasswordEncoded
     */
    public void setUserPasswordEncoded(boolean userPasswordEncoded)
    {
        isUserPasswordEncoded = userPasswordEncoded;
    }
}
