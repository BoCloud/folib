package com.veadan.folib.controllers;

import javax.servlet.http.HttpServletResponse;

import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

/**
 * @author Steve Todorov
 */
@Controller
public class UiController implements ErrorController
{

    @GetMapping(path = { "/**", "/error" }, produces = { MediaType.TEXT_HTML_VALUE })
    public String indexWithRoute(HttpServletResponse response)
    {
        response.setStatus(HttpStatus.NOT_FOUND.value());
        
        return "/index.html";
    }

    @GetMapping(path = { "/" }, produces = { MediaType.TEXT_HTML_VALUE })
    public String index()
    {
        return "/index.html";
    }

    @Override
    public String getErrorPath()
    {
        return "/error";
    }

    
}
