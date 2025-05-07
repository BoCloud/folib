package com.veadan.folib.components.ws;

import com.veadan.folib.cluster.FolibLockProperties;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.security.SecurityComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.PrivilegeDispatch;
import com.veadan.folib.event.privilege.PrivilegeEventTypeEnum;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.glassfish.jersey.client.ClientProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StreamUtils;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;


import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.util.*;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class WSForwardComponent {

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    @Autowired
    private FolibLockProperties folibLockProperties;

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Autowired
    private SecurityComponent securityComponent;

    @Autowired
    @Lazy
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Autowired
    private RestTemplate restTemplate;

    private final String DISPATCH_API_ENDPOINT = "/api/artifact/folib/promotion/artifactDispatch";

    private final String AUTH_DISPATCH_API_ENDPOINT = "/api/auth/privilegeDispatch";

    /**
     * WS请求转发
     *
     * @param targetNodeName 目标节点名称
     * @return true 已转发 其他值发生错误
     * @throws Exception 异常
     */
    public boolean forward(String targetNodeName) throws Exception {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        if (Objects.isNull(requestAttributes)) {
            throw new BusinessException("RequestAttributes is null");
        }
        String nodeName = folibWsRunManageV2.getSimpleTargetHostName(targetNodeName);
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
        HttpServletResponse response = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getResponse();
        String key = GlobalConstants.WS_NODE_KEY;
        String wsNodes = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(wsNodes)) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String currentHost = folibLockProperties.getFolibLockIp();
        List<String> wsNodeList = Arrays.asList(wsNodes.split(","));
        log.info("Current wsNodeList {} targetNode [{}]", wsNodeList, nodeName);
        Optional<String> nodeUrlOptional = wsNodeList.stream().filter(item -> item.startsWith(nodeName) && !currentHost.equals(UrlUtils.getHost(item.split("_")[1]))).findFirst();
        if (nodeUrlOptional.isEmpty()) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String nodeUrl = nodeUrlOptional.get();
        if (StringUtils.isBlank(nodeUrl)) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        nodeUrl = nodeUrl.split("_")[1];
        String host = UrlUtils.getHost(nodeUrl);
        if (currentHost.equals(host)) {
            //代理目标为本机，跳过
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String requestURI = request.getRequestURI();
        log.info("接收到转发请求...URI [{}] 目标 [{}]", requestURI, nodeUrl);
        // 请求类型
        String method = request.getMethod();
        // 请求方式
        HttpMethod httpMethod = HttpMethod.valueOf(method);
        // 请求头
        MultiValueMap<String, String> headers = parseRequestHeader(request);
        String params = getParamsString(request);
        // 请求体
        byte[] body = parseRequestBody(request);
        String url = StringUtils.removeEnd(nodeUrl, GlobalConstants.SEPARATOR) + requestURI;
        if (StringUtils.isNotBlank(params)) {
            url = url + "?" + params;
        }
        // 封装发http请求
        RequestEntity requestEntity = new RequestEntity(body, headers, httpMethod, URI.create(url));
        ResponseEntity<String> result = restTemplate.exchange(requestEntity, String.class);
        // 将转发请求得到的结果和响应头返回客户端
        String resultBody = result.getBody();
        log.info("转发请求...URI [{}] 目标 [{}] 状态 [{}] 结果 [{}]", requestURI, nodeUrl, result.getStatusCode(), resultBody);
        if (HttpStatus.OK != result.getStatusCode()) {
            log.error("转发请求失败...URI [{}] 目标 [{}] 状态 [{}] 结果 [{}]", requestURI, nodeUrl, result.getStatusCode(), resultBody);
            throw new BusinessException("ws forward error");
        }
        HttpHeaders resultHeaders = result.getHeaders();
        MediaType contentType = resultHeaders.getContentType();
        if (contentType != null) {
            response.setContentType(contentType.toString());
        }
        // 在getWriter之前执行，否则不生效
        response.setCharacterEncoding("UTF-8");
        PrintWriter writer = response.getWriter();
        writer.write(resultBody);
        writer.flush();
        log.info("转发请求...URI [{}] 目标 [{}] 成功", requestURI, nodeUrl);
        return true;
    }

    /**
     * 分发
     *
     * @param targetNodeName   目标节点名称
     * @param artifactDispatch 分发参数
     * @return true 已处理 其他值发生错误
     */
    public boolean dispatch(String targetNodeName, ArtifactDispatch artifactDispatch) {
        String key = GlobalConstants.WS_NODE_KEY;
        String wsNodes = distributedCacheComponent.get(key);
        String nodeName = folibWsRunManageV2.getSimpleTargetHostName(targetNodeName);
        if (StringUtils.isBlank(wsNodes)) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String currentHost = folibLockProperties.getFolibLockIp();
        List<String> wsNodeList = Arrays.asList(wsNodes.split(","));
        log.info("Current wsNodeList {} targetNode [{}]", wsNodeList, nodeName);
        Optional<String> nodeUrlOptional = wsNodeList.stream().filter(item -> item.startsWith(nodeName) && !currentHost.equals(UrlUtils.getHost(item.split("_")[1]))).findFirst();
        if (nodeUrlOptional.isEmpty()) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String nodeUrl = nodeUrlOptional.get();
        if (StringUtils.isBlank(nodeUrl)) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        nodeUrl = nodeUrl.split("_")[1];
        String host = UrlUtils.getHost(nodeUrl);
        if (currentHost.equals(host)) {
            //代理目标为本机，跳过
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        log.info("接收到分发请求，目标[{}]...", nodeUrl);
        Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient();
        //连接建立超时时间
        client.property(ClientProperties.CONNECT_TIMEOUT, 10000);
        String targetUrl = StringUtils.removeEnd(nodeUrl, GlobalConstants.SEPARATOR) + DISPATCH_API_ENDPOINT;
        WebTarget target = client.target(targetUrl);
        Invocation.Builder builder = target.request(javax.ws.rs.core.MediaType.APPLICATION_JSON);
        securityComponent.securityTokenHeader(builder);
        Response response = builder.post(Entity.entity(artifactDispatch, javax.ws.rs.core.MediaType.APPLICATION_JSON));
        String responseBody = response.readEntity(String.class);
        if (org.apache.http.HttpStatus.SC_OK != response.getStatus()) {
            log.error("Url response error [{}] [{}] [{}]", targetUrl, response.getStatus(), responseBody);
            throw new RuntimeException(String.format("Url response error [%s] [%s]", response.getStatus(), responseBody));
        }
        log.info("转发分发请求成功 目标[{}]...", nodeUrl);
        return true;
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

    public boolean dispatch(String targetNodeName, PrivilegeDispatch privilegeDispatch) {
        String key = GlobalConstants.WS_NODE_KEY;
        String wsNodes = distributedCacheComponent.get(key);
        log.info("Current wsNodes {}", wsNodes);
        String nodeName = folibWsRunManageV2.getSimpleTargetHostName(targetNodeName);
        if (StringUtils.isBlank(wsNodes)) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String currentHost = folibLockProperties.getFolibLockIp();
        List<String> wsNodeList = Arrays.asList(wsNodes.split(","));
        log.info("Current wsNodeList {} targetNode [{}]", wsNodeList, nodeName);
        Optional<String> nodeUrlOptional = wsNodeList.stream().filter(item -> item.startsWith(nodeName) && !currentHost.equals(UrlUtils.getHost(item.split("_")[0]))).findFirst();
        if (nodeUrlOptional.isEmpty()) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        String nodeUrl = nodeUrlOptional.get();
        if (StringUtils.isBlank(nodeUrl)) {
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        nodeUrl = nodeUrl.split("_")[0];
        String host = UrlUtils.getHost(nodeUrl);
        if (currentHost.equals(host)) {
            //代理目标为本机，跳过
            throw new RuntimeException("Not found session with targetHostName:" + nodeName);
        }
        log.info("接收到分发请求，目标[{}]...", nodeUrl);
        Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient();
        //连接建立超时时间
        client.property(ClientProperties.CONNECT_TIMEOUT, 10000);
        String targetUrl = StringUtils.removeEnd(nodeUrl, GlobalConstants.SEPARATOR) + AUTH_DISPATCH_API_ENDPOINT;
        WebTarget target = client.target(targetUrl);
        Invocation.Builder builder = target.request(javax.ws.rs.core.MediaType.APPLICATION_JSON);
        securityComponent.securityTokenHeader(builder);
        Response response = builder.post(Entity.entity(privilegeDispatch, javax.ws.rs.core.MediaType.APPLICATION_JSON));
        String responseBody = response.readEntity(String.class);
        if (org.apache.http.HttpStatus.SC_OK != response.getStatus()) {
            log.error("Url response error [{}] [{}] [{}]", targetUrl, response.getStatus(), responseBody);
            throw new RuntimeException(String.format("Url response error [%s] [%s]", response.getStatus(), responseBody));
        }
        log.info("转发分发请求成功 目标[{}]...", nodeUrl);
        return true;
    }

    public boolean dispatchTargetNode(String targetNodeName, PrivilegeDispatch privilegeDispatch) {
        log.info("接收到分发请求，目标[{}]...", targetNodeName);
        Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient();
        //连接建立超时时间
        client.property(ClientProperties.CONNECT_TIMEOUT, 10000);
        String targetUrl = StringUtils.removeEnd(targetNodeName, GlobalConstants.SEPARATOR) + AUTH_DISPATCH_API_ENDPOINT;
        WebTarget target = client.target(targetUrl);
        Invocation.Builder builder = target.request(javax.ws.rs.core.MediaType.APPLICATION_JSON);
        securityComponent.securityTokenHeader(builder);
        Response response = builder.post(Entity.entity(privilegeDispatch, javax.ws.rs.core.MediaType.APPLICATION_JSON));
        String responseBody = response.readEntity(String.class);
        if (org.apache.http.HttpStatus.SC_OK != response.getStatus()) {
            log.error("Url response error [{}] [{}] [{}]", targetUrl, response.getStatus(), responseBody);
            throw new RuntimeException(String.format("Url response error [%s] [%s]", response.getStatus(), responseBody));
        }
        log.info("转发分发请求成功 目标[{}]...", targetNodeName);
        return true;
    }
}
