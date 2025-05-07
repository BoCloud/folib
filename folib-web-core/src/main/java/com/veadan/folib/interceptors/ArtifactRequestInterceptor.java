package com.veadan.folib.interceptors;

import static com.veadan.folib.web.Constants.REPOSITORY_REQUEST_ATTRIBUTE;

import java.io.IOException;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.HandlerMapping;


/**
 * @author xuxinping
 *
 */
public abstract class ArtifactRequestInterceptor implements HandlerInterceptor
{

    private final String layout;
    
    public ArtifactRequestInterceptor(String layout)
    {
        this.layout = layout;
    }


    public final boolean preHandle(HttpServletRequest request,
                                   HttpServletResponse response,
                                   Object handler)
        throws Exception
    {
        final Map pathVariables = (Map) request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE);
        if (pathVariables == null)
        {
            return true;
        }

        final Repository repository = (Repository) request.getAttribute(REPOSITORY_REQUEST_ATTRIBUTE);
        if (repository == null || !layout.equals(repository.getLayout()))
        {
            return true;
        }

        final String artifactPath = (String) pathVariables.get("artifactPath");
        if (StringUtils.isBlank(artifactPath))
        {
            return true;
        }

        return preHandle(repository, artifactPath, request, response);
    }

    protected abstract boolean preHandle(Repository repository,
                                         String artifactPath,
                                         HttpServletRequest request,
                                         HttpServletResponse response) throws IOException;

}
