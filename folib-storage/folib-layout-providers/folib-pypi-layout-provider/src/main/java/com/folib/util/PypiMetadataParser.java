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
