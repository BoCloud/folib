package com.folib.controllers;

import com.folib.constant.GlobalConstants;
import io.swagger.annotations.Api;
import org.apache.commons.lang.StringUtils;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

/**
 * @author Steve Todorov
 */
@Controller
@Api(description = "跳转首页/错误页控制", tags = "跳转首页/错误页控制")
public class UiController implements ErrorController {

    //@GetMapping(path = {"/robots.txt"})
    //public ResponseEntity robots(HttpServletResponse response) {
    //    return ResponseEntity.status(HttpStatus.NOT_FOUND.value()).build();
    //}

    //@GetMapping(path = {"/**"}, produces = {MediaType.TEXT_HTML_VALUE})
    //public String indexWithRoute(HttpServletRequest request, HttpServletResponse response) {
    //    String path = request.getRequestURI();
    //    if (path.startsWith("/error")) {
    //        // 适配webdav 能够401返回给客户端而不发生重定向
    //        return null;
    //    }
    //    response.setStatus(HttpStatus.NOT_FOUND.value());
    //    return String.format("redirect:%s",getUIIndex());
    //}

    @GetMapping(path = {"/"}, produces = {MediaType.TEXT_HTML_VALUE})
    public String index() {

        return String.format("redirect:%s",getUIIndex());
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
