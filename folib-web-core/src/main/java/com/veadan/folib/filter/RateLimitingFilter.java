package com.veadan.folib.filter;

import com.veadan.folib.config.Bucket4jConfig;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@Slf4j
@Component
public class RateLimitingFilter extends OncePerRequestFilter {

    private static final long SLOW_REQUEST_THRESHOLD_MS = 1000;

    @Autowired
    private Bucket4jConfig bucket4jConfig;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
            throws ServletException, IOException {
//        long startTime = System.currentTimeMillis();
        if (bucket4jConfig.getBucket().tryConsume(1)) {
            try {
                // 继续执行后续过滤器链
                filterChain.doFilter(request, response);
            } finally {
//                // 计算请求处理时间
//                long duration = System.currentTimeMillis() - startTime;
//                // 如果请求处理时间超过阈值，则认为是慢请求，记录日志
//                if (duration > SLOW_REQUEST_THRESHOLD_MS) {
//                    log.warn("慢请求: {} {} 耗时 {} ms，状态码：{}", request.getMethod(), request.getRequestURI(), duration, response.getStatus());
//                }
            }
        } else {
            log.warn("Rate limit exceeded [{}]", request.getRequestURI());
            response.setContentType("application/json");
            response.setStatus(HttpStatus.TOO_MANY_REQUESTS.value());
            response.getWriter().write("{ \"error\": \"Rate limit exceeded\" }");
        }
    }
}