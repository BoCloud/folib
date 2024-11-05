package com.veadan.folib.filter;

import com.veadan.folib.components.thirdparty.foeyes.FoEyesProperties;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.*;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StreamUtils;
import org.springframework.web.client.RestTemplate;

import javax.inject.Inject;
import javax.servlet.*;
import javax.servlet.annotation.WebFilter;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

/**
 * @author leipenghui
 * @date 2024/4/25
 **/
@WebFilter(urlPatterns = "/dependency/*", filterName = "foEyesHttpProxyFilter")
@Slf4j
public class FoEyesHttpProxyFilter implements Filter {

    @Inject
    private FoEyesProperties foEyesProperties;

    @Inject
    private RestTemplate restTemplate;

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        log.info("[ {} ] 初始化网关...", this.getClass().getSimpleName());
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain filterChain) throws IOException, ServletException {
        HttpServletRequest req = (HttpServletRequest) request;
        String requestURI = req.getRequestURI();
        String allowedPath = "/dependency/";
        if (!requestURI.startsWith(allowedPath)) {
            filterChain.doFilter(request, response);
            return;
        }
        log.info("[ {} ] 接收到请求...URI:{}", this.getClass().getSimpleName(), requestURI);
        HttpServletResponse resp = (HttpServletResponse) response;
        // 请求类型
        String method = req.getMethod();
        // 请求方式
        HttpMethod httpMethod = HttpMethod.resolve(method);
        // 请求头
        MultiValueMap<String, String> headers = parseRequestHeader(req);
        String params = getParamsString(req);
        // 请求体
        byte[] body = parseRequestBody(req);
        requestURI = requestURI.replace(allowedPath, "/api/");
        String url = foEyesProperties.getBaseUrl() + requestURI;
        if (StringUtils.isNotBlank(params)) {
            url = url + "?" + params;
        }
        // 封装发http请求
        RequestEntity requestEntity = new RequestEntity(body, headers, httpMethod, URI.create(url));
        ResponseEntity<String> result = restTemplate.exchange(requestEntity, String.class);
        // 将转发请求得到的结果和响应头返回客户端
        String resultBody = result.getBody();
        HttpHeaders resultHeaders = result.getHeaders();
        MediaType contentType = resultHeaders.getContentType();
        if (contentType != null) {
            resp.setContentType(contentType.toString());
        }
        // 设置响应头部
        resultHeaders.forEach((headerName, headerValues) -> {
            for (String headerValue : headerValues) {
                resp.addHeader(headerName, headerValue);
            }
        });
        // 在getWriter之前执行，否则不生效
        resp.setCharacterEncoding("UTF-8");
        PrintWriter writer = resp.getWriter();
        writer.write(resultBody);
        writer.flush();
    }

    @Override
    public void destroy() {
        log.info("[ {} ] 关闭网关...", this.getClass().getSimpleName());
    }

    /**
     * request header
     *
     * @param request 请求
     * @return 结果
     */
    private MultiValueMap<String, String> parseRequestHeader(HttpServletRequest request) {
        HttpHeaders httpHeaders = new HttpHeaders();
        List<String> headerNames = Collections.list(request.getHeaderNames());
        for (String headerName : headerNames) {
            List<String> headerValues = Collections.list(request.getHeaders(headerName));
            for (String headerValue : headerValues) {
                httpHeaders.add(headerName, headerValue);
            }
        }
        return httpHeaders;
    }

    /**
     * request body
     *
     * @param request 请求
     * @return 结果
     * @throws IOException 异常
     */
    private byte[] parseRequestBody(HttpServletRequest request) throws IOException {
        InputStream inputStream = request.getInputStream();
        return StreamUtils.copyToByteArray(inputStream);
    }

    /**
     * 获取请求参数
     *
     * @param request 请求
     * @return 请求参数
     */
    public static String getParamsString(HttpServletRequest request) {
        StringBuilder params = new StringBuilder();
        Enumeration<String> paramNames = request.getParameterNames();
        while (paramNames.hasMoreElements()) {
            String paramName = (String) paramNames.nextElement();
            String[] values = request.getParameterValues(paramName);
            for (int i = 0; i < values.length; i++) {
                String value = values[i];
                params.append(paramName);
                if (value != null && value.trim().length() > 0) {
                    params.append("=").append(value);
                }
                if (i < values.length - 1 || paramNames.hasMoreElements()) {
                    params.append("&");
                }
            }
        }
        return params.toString();
    }
}