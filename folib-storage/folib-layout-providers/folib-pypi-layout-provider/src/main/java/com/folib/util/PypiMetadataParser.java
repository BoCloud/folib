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
package com.folib.util;

import com.folib.domain.PypiPackageInfo;
import com.folib.util.annotations.PypiMetadataKey;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.validation.ConstraintViolationException;
import java.io.*;
import java.lang.reflect.Field;
import java.util.Map;
import java.util.TreeMap;

@Component
public class PypiMetadataParser
{

    private static final Logger logger = LoggerFactory.getLogger(PypiMetadataParser.class);

    @Autowired
    PypiPackageInfoValidator pypiPackageInfoValidator;

    public PypiPackageInfo parseMetadataFile(InputStream is)
            throws IllegalAccessException, IOException, ConstraintViolationException
    {
        Map<String, String> keyValueMap = generateKeyValueMap(is);
        PypiPackageInfo packageInfo = populateAnnotatedFields(new PypiPackageInfo(), keyValueMap);
        pypiPackageInfoValidator.validate(packageInfo);
        return packageInfo;
    }

    public PypiPackageInfo populateAnnotatedFields(PypiPackageInfo object,
                                                   Map<String, String> keyValueMap)
            throws IllegalAccessException
    {
        Class<?> clazz = object.getClass();
        for (Field field : clazz.getDeclaredFields())
        {
            field.setAccessible(true);
            try
            {
                if (field.isAnnotationPresent(PypiMetadataKey.class))
                {
                    PypiMetadataKey pypiMetadataKey = field.getAnnotation(PypiMetadataKey.class);
                    if (pypiMetadataKey.name().equals("Metadata-Version"))
                    {
                        field.set(object, PypiPackageInfo.SupportedMetadataVersionEnum
                                                  .getVersionEnum(keyValueMap.get(pypiMetadataKey.name())));
                    }
                    else
                    {
                        field.set(object, keyValueMap.get(pypiMetadataKey.name()));
                    }
                }
            }
            catch (IllegalAccessException ex)
            {
                logger.error("Exception occurred ", ex);
                throw ex;
            }
        }
        return object;
    }

    private Map<String, String> generateKeyValueMap(InputStream is)
            throws IOException
    {
        Map<String, String> keyValueMap = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

        BufferedReader reader;
        try
        {
            reader = new BufferedReader(new InputStreamReader(is));

            String line = "";
            while ((line = reader.readLine()) != null)
            {
                String[] keysValues = line.split(":", 2);
                keyValueMap.put(keysValues[0].trim(), keysValues[1].trim());
            }
            reader.close();
        }
        catch (IOException ex)
        {
            logger.error("Exception occurred ", ex);
            throw ex;
        }
        return keyValueMap;
    }

}
