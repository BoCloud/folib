package com.veadan.folib.filter;

import com.google.common.collect.Sets;
import com.veadan.folib.wrapper.RequestWrapper;

import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Collections;
import java.util.Set;

/**
 * @author leipenghui
 **/
@WebFilter(urlPatterns = "/api/artifact/folib/promotion/*")
public class WrapperRequestFilter implements Filter {

    private static final Set<String> FILTER_PATHS = Collections.unmodifiableSet(Sets.newHashSet("/api/artifact/folib/promotion/"));

    private static final Set<String> SLICE_PATH = Collections.unmodifiableSet(Sets.newHashSet("/api/artifact/folib/promotion/slice/upload-web"));

    private static final Set<String> ALLOWED_PATHS = Collections.unmodifiableSet(Sets.newHashSet("/api/artifact/folib/promotion/upload-files", "/api/artifact/folib/promotion/parseArtifact"
            , "/api/artifact/folib/promotion/slice/upload"
            , "/api/artifact/folib/promotion/header/slice/upload"
    ));

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {

    }

    @Override
    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain) throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) servletRequest;
        String path = request.getRequestURI().substring(request.getContextPath().length()).replaceAll("[/]+$", "");
        boolean allowedPath = ALLOWED_PATHS.contains(path);
        if (allowedPath) {
            filterChain.doFilter(servletRequest, servletResponse);
            return;
        }else if(SLICE_PATH.stream().anyMatch(path::equals)){
            filterChain.doFilter(servletRequest, servletResponse);
            return;
        }
        else if (FILTER_PATHS.stream().anyMatch(path::startsWith)){
            ServletRequest requestWrapper = new RequestWrapper((HttpServletRequest) servletRequest);
            // 将请求封装并传递下去
            filterChain.doFilter(requestWrapper, servletResponse);
            return;
        }
        filterChain.doFilter(servletRequest, servletResponse);
    }

    @Override
    public void destroy() {

    }
}
