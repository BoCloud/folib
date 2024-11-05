package com.veadan.folib.components;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import java.util.Objects;

/**
 * @author leipenghui
 * @date 2024/1/25
 **/
@Slf4j
@Component
public class DockerClientComponent {

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    public ResponseResult doGet(String storageId, String repositoryId, String targetUrl, MultivaluedMap<String, Object> headers, boolean basicAuth) {
        log.debug("StorageId [{}] repositoryId [{}] targetUrl [{}] headers [{}]", storageId, repositoryId, targetUrl, MapUtils.isNotEmpty(headers) ? JSONObject.toJSONString(headers) : null);
        Response response = null;
        Client client = null;
        try {
            String username = "", password = "";
            if (StringUtils.isNotBlank(storageId) && StringUtils.isNotBlank(repositoryId)) {
                Repository repository = configurationManager.getRepository(storageId, repositoryId);
                if (Objects.nonNull(repository)) {
                    RemoteRepository remoteRepository = repository.getRemoteRepository();
                    if (Objects.nonNull(remoteRepository)) {
                        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
                            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", storageId, repositoryId, remoteRepository.getUrl());
                            return null;
                        }
                        if (StringUtils.isNotBlank(remoteRepository.getUsername())) {
                            username = remoteRepository.getUsername();
                        }
                        if (StringUtils.isNotBlank(remoteRepository.getPassword())) {
                            password = remoteRepository.getPassword();
                        }
                    }
                }
            }
            client = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            //连接建立超时时间
            client.property(ClientProperties.CONNECT_TIMEOUT, 10000);
            //读取内容超时时间
            client.property(ClientProperties.READ_TIMEOUT, 60000);
            WebTarget target = client.target(targetUrl);
            if (basicAuth) {
                final HttpAuthenticationFeature authenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? HttpAuthenticationFeature.basic(username, password) : null;
                if (Objects.nonNull(authenticationFeature)) {
                    target.register(authenticationFeature);
                }
            }
            Invocation.Builder builder = target.request();
            if (MapUtils.isNotEmpty(headers)) {
                builder = builder.headers(headers);
            }
            response = builder.get();
            String responseBody = response.readEntity(String.class);
            if (HttpStatus.SC_OK != response.getStatus()) {
                log.debug("Url response error [{}] [{}] [{}] [{}]", targetUrl, headers, response.getStatus(), responseBody);
            }
            ResponseResult responseResult = ResponseResult.builder().build();
            responseResult.setHttpStatus(response.getStatus());
            responseResult.setData(responseBody);
            responseResult.setDataType(ResponseDataTypeEnum.STRING);
            if (JSONUtil.isJson(responseBody)) {
                responseResult.setDataType(ResponseDataTypeEnum.JSON);
            }
            responseResult.setHeaders(response.getStringHeaders());
            log.debug("ResponseResult [{}] [{}]", targetUrl, JSONObject.toJSONString(responseResult));
            return responseResult;
        } catch (Exception ex) {
            log.error("Get targetUrl [{}] error [{}]", targetUrl, ExceptionUtils.getStackTrace(ex));
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
            if (Objects.nonNull(client)) {
                client.close();
            }
        }
        return null;
    }

    public ResponseResult doGet(String targetUrl) {
        return doGet(null, null, targetUrl, null, true);
    }

    public ResponseResult doGet(String targetUrl, MultivaluedMap<String, Object> headers) {
        return doGet(null, null, targetUrl, headers, true);
    }

    public ResponseResult doGet(String storageId, String repositoryId, String targetUrl) {
        return doGet(storageId, repositoryId, targetUrl, null, true);
    }

    public ResponseResult doGet(String storageId, String repositoryId, String targetUrl, boolean basicAuth) {
        return doGet(storageId, repositoryId, targetUrl, null, basicAuth);
    }
}
