package com.veadan.folib.utils;

import javax.servlet.http.HttpServletRequest;

import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * @author 
 */
public class UrlUtils
{

    private UrlUtils()
    {
    }

    public static String getRequestUri()
    {
        HttpServletRequest servletRequest = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getRequest();
        return servletRequest.getRequestURI();
    }

    public static String getCurrentStorageId()
    {
        return getSubPath(getRequestUri(), 2);
    }

    public static String getCurrentRepositoryId()
    {
        return getSubPath(getRequestUri(), 3);
    }

    private static String getSubPath(String url,
                                     int index)
    {
        String[] args = url.split("/");
        if (args.length < index + 1)
        {
            return null;
        }
        return args[index];
    }
}
