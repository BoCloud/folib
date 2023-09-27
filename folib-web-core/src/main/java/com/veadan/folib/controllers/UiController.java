package com.veadan.folib.controllers;

import io.swagger.annotations.Api;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.servlet.view.RedirectView;

import javax.servlet.http.HttpServletResponse;

/**
 * @author Steve Todorov
 */
@Controller
@Api(description = "跳转首页/错误页控制", tags = "跳转首页/错误页控制")
public class UiController implements ErrorController {

    @GetMapping(path = {"/**", "/error"}, produces = {MediaType.TEXT_HTML_VALUE})
    public RedirectView indexWithRoute(HttpServletResponse response) {
        response.setStatus(HttpStatus.NOT_FOUND.value());

        return new RedirectView("/ui/index.html", true, false);
    }

    @GetMapping(path = {"/"}, produces = {MediaType.TEXT_HTML_VALUE})
    public RedirectView index() {
        return new RedirectView("/ui/index.html", true, false);
    }

    @Override
    public String getErrorPath() {
        return "/error";
    }


}
