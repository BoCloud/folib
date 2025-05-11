//package com.veadan.folib.app.boot;
//
//import com.veadan.folib.app.boot.listeners.BootProgressAsyncListener;
//
//import java.io.IOException;
//import java.io.InputStream;
//import java.io.PrintWriter;
//import java.nio.file.Files;
//
//import com.veadan.folib.constant.GlobalConstants;
//import io.reactivex.Observable;
//import io.reactivex.disposables.Disposable;
//import jakarta.servlet.AsyncContext;
//import jakarta.servlet.ServletException;
//import jakarta.servlet.http.HttpServlet;
//import jakarta.servlet.http.HttpServletRequest;
//import jakarta.servlet.http.HttpServletResponse;
//import org.apache.commons.io.IOUtils;
//import org.apache.commons.lang3.StringUtils;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//import org.springframework.http.HttpStatus;
//import org.springframework.http.MediaType;
//
///**
// * <p>
// * This servlet is used to process requests via jetty in the temporary web server started with {@link BootProgressBeanPostProcessor}
// * </p>
// *
// * <p>
// * WARNING: Do not be tempted to inline this class into {@link BootProgressServlet} - you'll get this error:
// * </p>
// * <pre>Caused by: java.lang.NoSuchMethodException: BootProgressBeanPostProcessor$BootProgressServlet.&lt;init&gt;()</pre>
// */
//class BootProgressServlet
//        extends HttpServlet
//{
//
//    private static final Logger logger = LoggerFactory.getLogger(BootProgressServlet.class);
//
//    public static final String pingRequestURI = "/api/ping";
//    public static final String assetsRequestURI = "/ui/";
//
//    private final Observable<String> observable;
//
//    public BootProgressServlet(Observable<String> observable)
//    {
//        this.observable = observable;
//    }
//
//    @Override
//    protected void doGet(HttpServletRequest request,
//                         HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        if (request.getRequestURI().equalsIgnoreCase(pingRequestURI))
//        {
//            processPingRequest(request, response);
//        }
//        else
//        {
//            processAssetResourceRequest(request, response);
//        }
//    }
//
//    private void serviceUnavailable(HttpServletResponse response)
//    {
//        response.setStatus(HttpStatus.SERVICE_UNAVAILABLE.value());
//    }
//
//    @Override
//    protected void doHead(HttpServletRequest request,
//                          HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        serviceUnavailable(response);
//    }
//
//    @Override
//    protected void doPost(HttpServletRequest request,
//                          HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        serviceUnavailable(response);
//    }
//
//    @Override
//    protected void doPut(HttpServletRequest request,
//                         HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        serviceUnavailable(response);
//    }
//
//    @Override
//    protected void doDelete(HttpServletRequest request,
//                            HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        serviceUnavailable(response);
//    }
//
//    @Override
//    protected void doOptions(HttpServletRequest request,
//                             HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        serviceUnavailable(response);
//    }
//
//    @Override
//    protected void doTrace(HttpServletRequest request,
//                           HttpServletResponse response)
//            throws ServletException, IOException
//    {
//        serviceUnavailable(response);
//    }
//
//    private void processAssetResourceRequest(HttpServletRequest request,
//                                             HttpServletResponse response)
//            throws IOException
//    {
//        try
//        {
//            String requestedUri = request.getRequestURI();
//
//            // Strip context path from the beginning.
//            if (StringUtils.isNotBlank(getServletContext().getContextPath()))
//            {
//                requestedUri = requestedUri.substring(getServletContext().getContextPath().length());
//            }
//
//            String requestedResource = "/index.html";
//
//            // Set default status code for all requests
//            response.setStatus(HttpStatus.SERVICE_UNAVAILABLE.value());
//            String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
//            if (StringUtils.isBlank(webUrlPrefix)) {
//                webUrlPrefix = assetsRequestURI;
//            }
//            // Serve assets
//            if (requestedUri.startsWith(webUrlPrefix))
//            {
//                response.setStatus(HttpStatus.OK.value());
//                requestedResource = requestedUri;
//            }
//
//            readAssetResource(requestedResource, response);
//        }
//        catch (Exception e)
//        {
//            logger.error(e.getMessage(), e);
//            response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value());
//        }
//    }
//
//
//    private void readAssetResource(String requestedResource,
//                                   HttpServletResponse response)
//    {
//        try (InputStream inputStream = BootProgressServlet.class.getResourceAsStream(requestedResource))
//        {
//            String mime = MediaType.APPLICATION_OCTET_STREAM_VALUE;
//
//            if (requestedResource.endsWith(".html"))
//            {
//                mime = MediaType.TEXT_HTML_VALUE;
//            }
//            else if (requestedResource.endsWith(".css"))
//            {
//                mime = "text/css";
//            }
//            else if (requestedResource.endsWith(".js"))
//            {
//                mime = "application/javascript";
//            }
//
//            response.setContentType(mime);
//
//            response.flushBuffer();
//
//            IOUtils.copy(inputStream, response.getOutputStream());
//        }
//        catch (IOException e)
//        {
//            logger.warn("Failed to read file from classpath!", e);
//            response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
//        }
//    }
//
//
//    private void processPingRequest(HttpServletRequest request,
//                                    HttpServletResponse response)
//            throws IOException
//    {
//        try
//        {
//            if (!MediaType.TEXT_EVENT_STREAM_VALUE.equalsIgnoreCase(request.getHeader("Accept")))
//            {
//                response.sendError(HttpStatus.NOT_ACCEPTABLE.value());
//                return;
//            }
//
//            response.setStatus(HttpStatus.OK.value());
//            response.setContentType(MediaType.TEXT_EVENT_STREAM_VALUE);
//            response.setCharacterEncoding("UTF-8");
//
//            response.setHeader("Connection", "keep-alive");
//            response.setHeader("Cache-control", "no-cache, no-store, max-age=0, must-revalidate");
//            response.setHeader("Pragma", "no-cache");
//            response.setHeader("Expires", "0");
//            // -- CORS - allow everybody while booting, but set max-age to
//            // force browsers re-check after boot.
//            response.setHeader("Allow", "GET, OPTIONS");
//            response.setHeader("Access-Control-Allow-Origin", "*");
//            response.setHeader("Access-Control-Allow-Headers", "*");
//            response.setHeader("Access-Control-Max-Age", "600");
//
//            final AsyncContext asyncContext = request.startAsync(request, response);
//
//            final Disposable disposable = observable.doOnComplete(() -> {
//                emitBootEvent("booted", "", asyncContext);
//                asyncContext.getResponse().getWriter().flush();
//                asyncContext.complete();
//            }).doOnError((e) -> {
//                logger.error("An error occurred!", e);
//                asyncContext.complete();
//            }).subscribe(data -> emitBootEvent("booting", data, asyncContext));
//
//            asyncContext.addListener(new BootProgressAsyncListener(disposable));
//        }
//        catch (Exception e)
//        {
//            logger.error("Failed to process request {}", request.getRequestURI(), e);
//            response.sendError(HttpStatus.INTERNAL_SERVER_ERROR.value());
//        }
//    }
//
//    private void emitBootEvent(String event,
//                               String data,
//                               AsyncContext asyncContext)
//            throws IOException
//    {
//        PrintWriter writer = asyncContext.getResponse().getWriter();
//        writer.println(String.format("event: %s", event));
//        writer.println(String.format("data: %s", data));
//        writer.println();
//        writer.println();
//        writer.flush();
//    }
//
//}
