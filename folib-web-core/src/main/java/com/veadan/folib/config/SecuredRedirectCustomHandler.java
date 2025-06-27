//package com.veadan.folib.config;
//
//import jakarta.servlet.ServletException;
//
//import org.eclipse.jetty.ee10.servlet.ServletCoreRequest;
//import org.eclipse.jetty.http.HttpHeader;
//import org.eclipse.jetty.server.*;
//import org.eclipse.jetty.server.handler.AbstractHandler;
//import org.eclipse.jetty.server.internal.HttpConnection;
//import org.eclipse.jetty.util.Callback;
//import org.eclipse.jetty.util.URIUtil;
//import org.springframework.web.servlet.mvc.HttpRequestHandlerAdapter;
//
//import jakarta.servlet.http.HttpServletRequest;
//import jakarta.servlet.http.HttpServletResponse;
//import java.io.IOException;
//
//
//import java.io.IOException;
//import java.nio.ByteBuffer;
//
///**
// * @author veadan
// * @date 2024/12/5
// **/
//public class SecuredRedirectCustomHandler extends AbstractHandler {
//
//    public SecuredRedirectCustomHandler() {
//    }
//
//    //@Override
//    //public void handle(String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
//    //    HttpChannel channel = baseRequest.getHttpChannel();
//    //    if (!baseRequest.isSecure() && channel != null) {
//    //        HttpConfiguration httpConfig = channel.getHttpConfiguration();
//    //        if (httpConfig == null) {
//    //            response.sendError(403, "No http configuration available");
//    //        } else {
//    //            if (httpConfig.getSecurePort() > 0) {
//    //                String scheme = httpConfig.getSecureScheme();
//    //                int port = httpConfig.getSecurePort();
//    //                String url = URIUtil.newURI(scheme, baseRequest.getServerName(), port, baseRequest.getRequestURI(), baseRequest.getQueryString());
//    //                response.setContentLength(0);
//    //                baseRequest.getResponse().sendRedirect(307, url, true);
//    //            } else {
//    //                response.sendError(403, "Not Secure");
//    //            }
//    //            baseRequest.setHandled(true);
//    //        }
//    //    }
//    //}
//
//
//    @Override
//    public boolean handle(Request request, Response response, Callback callback) throws Exception {
//        // 1. 判断是否安全连接
//        if (!request.isSecure()) {
//            // 2. 获取HTTP配置（适配Jetty 12 API）
//            ConnectionMetaData metaData = request.getConnectionMetaData();
//            HttpConfiguration httpConfig = (metaData instanceof HttpConnection connection) ?
//                    connection.getHttpConfiguration() : null;
//
//            if (httpConfig == null) {
//                response.setStatus(403);
//                response.write(true,
//                        ByteBuffer.wrap("No http configuration available".getBytes()),
//                        callback);
//                return true;
//            }
//
//            // 3. 构建重定向URL（适配新URI构建方式）
//            String scheme = httpConfig.getSecureScheme();
//            int port = httpConfig.getSecurePort();
//            String host = request.getHttpURI().getHost();
//            String path = request.getHttpURI().getPath();
//            String query = request.getHttpURI().getQuery();
//
//            String url = URIUtil.newURI(
//                    scheme,
//                    host,
//                    port,
//                    path,
//                    query,
//                    null
//            );
//
//            // 4. 发送307重定向（异步回调处理）
//            response.setStatus(307);
//            response.getHeaders().add(HttpHeader.LOCATION, url);
//            response.write(true, ByteBuffer.allocate(0), callback);
//            return true;
//        }
//
//        // 非安全请求继续处理
//        return false;
//
//    }
//}