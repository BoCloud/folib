package com.veadan.folib.config;

import org.eclipse.jetty.server.HttpChannel;
import org.eclipse.jetty.server.HttpConfiguration;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.eclipse.jetty.util.URIUtil;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author leipenghui
 * @date 2024/12/5
 **/
public class SecuredRedirectCustomHandler extends AbstractHandler {

    public SecuredRedirectCustomHandler() {
    }

    @Override
    public void handle(String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        HttpChannel channel = baseRequest.getHttpChannel();
        if (!baseRequest.isSecure() && channel != null) {
            HttpConfiguration httpConfig = channel.getHttpConfiguration();
            if (httpConfig == null) {
                response.sendError(403, "No http configuration available");
            } else {
                if (httpConfig.getSecurePort() > 0) {
                    String scheme = httpConfig.getSecureScheme();
                    int port = httpConfig.getSecurePort();
                    String url = URIUtil.newURI(scheme, baseRequest.getServerName(), port, baseRequest.getRequestURI(), baseRequest.getQueryString());
                    response.setContentLength(0);
                    baseRequest.getResponse().sendRedirect(307, url, true);
                } else {
                    response.sendError(403, "Not Secure");
                }
                baseRequest.setHandled(true);
            }
        }
    }
}