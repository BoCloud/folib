package com.veadan.folib.controllers;

import com.veadan.folib.constant.GlobalConstants;
import io.swagger.annotations.Api;
import org.apache.commons.lang.StringUtils;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.servlet.view.RedirectView;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * @author Steve Todorov
 */
@Controller
@Api(description = "跳转首页/错误页控制", tags = "跳转首页/错误页控制")
public class UiController implements ErrorController {

    @GetMapping(path = {"/robots.txt"})
    public ResponseEntity robots(HttpServletResponse response) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND.value()).build();
    }

    @GetMapping(path = {"/**"}, produces = {MediaType.TEXT_HTML_VALUE})
    public RedirectView indexWithRoute(HttpServletRequest request, HttpServletResponse response) {
        String path = request.getRequestURI();
        if (path.startsWith("/error")) {
            // 适配webdav 能够401返回给客户端而不发生重定向
            return null;
        }
        response.setStatus(HttpStatus.NOT_FOUND.value());
        return new RedirectView(getUIIndex(), true, false);
    }

    @GetMapping(path = {"/"}, produces = {MediaType.TEXT_HTML_VALUE})
    public RedirectView index() {
        return new RedirectView(getUIIndex(), true, false);
    }


    public String getErrorPath() {
        return "/error";
    }

    private String getUIIndex() {
        String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
        if (StringUtils.isBlank(webUrlPrefix)) {
            webUrlPrefix = "/ui/";
        }
        return webUrlPrefix + "index.html";
    }



}
