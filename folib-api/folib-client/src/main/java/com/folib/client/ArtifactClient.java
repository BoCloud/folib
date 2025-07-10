/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.client;

import com.folib.config.CustomAuthenticationFeature;
import org.apache.http.HttpStatus;
import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.client.ClientConfig;
import org.springframework.security.authentication.AuthenticationServiceException;

import javax.ws.rs.ServerErrorException;
import javax.ws.rs.client.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.Closeable;
import java.io.InputStream;
import java.util.Map;

import static org.apache.http.HttpStatus.SC_FORBIDDEN;
import static org.apache.http.HttpStatus.SC_UNAUTHORIZED;

/**
 * @author veadan
 */
public class ArtifactClient extends BaseArtifactClient implements Closeable {

    private static final String HEADER_VALUE_MAVEN = "Maven/*";

    private static final String HEADER_NAME_USER_AGENT = "user-agent";

    protected String username;

    protected String password;

    private String protocol = "http";

    private String host = System.getProperty("folib.host") != null ?
            System.getProperty("folib.host") : "localhost";

    private int port = System.getProperty("folib.port") != null ?
            Integer.parseInt(System.getProperty("folib.port")) : 38080;

    private String contextBaseUrl;

    private Client client;


    public ArtifactClient() {
    }

    public static ArtifactClient getArtifactClientInstance(String username,
                                                           String password) {
        String host = System.getProperty("folib.host") != null ?
                System.getProperty("folib.host") :
                "localhost";

        int port = System.getProperty("folib.port") != null ?
                Integer.parseInt(System.getProperty("folib.port")) :
                38080;

        ArtifactClient client = new ArtifactClient();
        client.setUsername(username != null ? username : "deployer");
        client.setPassword(password != null ? password : "password");
        client.setPort(port);
        client.setContextBaseUrl("http://" + host + ":" + client.getPort());

        return client;
    }

    public Client getClientInstance() {
        if (client == null) {
            ClientConfig config = getClientConfig();
            client = ClientBuilder.newClient(config);
            return client;
        } else {
            return client;
        }
    }

    private ClientConfig getClientConfig() {
        ClientConfig config = new ClientConfig();
        config.connectorProvider(new ApacheConnectorProvider());
        return config;
    }

    @Override
    public void close() {
        if (client != null) {
            client.close();
        }
    }

    @Override
    public void deployFile(InputStream is,
                           String url,
                           String fileName)
            throws ArtifactOperationException {
        put(is, url, fileName, MediaType.APPLICATION_OCTET_STREAM);
    }

    @Override
    public void deployMetadata(InputStream is,
                               String url,
                               String fileName)
            throws ArtifactOperationException {
        put(is, url, fileName, MediaType.APPLICATION_XML);
    }

    @Override
    public void put(InputStream is,
                    String url,
                    String fileName,
                    String mediaType)
            throws ArtifactOperationException {
        String contentDisposition = "attachment; filename=\"" + fileName + "\"";

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(mediaType)
                .header("Content-Disposition", contentDisposition)
                .header(HEADER_NAME_USER_AGENT, HEADER_VALUE_MAVEN)
                .put(Entity.entity(is, mediaType));

        handleFailures(response, "Failed to upload file!");
    }


    @Override
    public InputStream getResource(String path,
                                   long offset) {
        return getResource(path, offset, null, null);
    }

    public InputStream getResource(String path,
                                   long offset,
                                   MediaType mediaType,
                                   Map<String, String> headers) {
        String url = getContextBaseUrl() + (!path.startsWith("/") ? "/" : "") + path;

        logger.info("Getting {}...", url);

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Invocation.Builder request = resource.request();
        if (mediaType != null) {
            resource.request(mediaType.getType());
        }

        if (headers != null) {
            for (Map.Entry<String, String> header : headers.entrySet()) {
                resource.request().header(header.getKey(), header.getValue());
            }
        }

        Response response;

        if (offset > 0) {
            response = request.header("Range", "bytes=" + offset + "-").get();
        } else {
            response = request.get();
        }

        return response.readEntity(InputStream.class);
    }

    public Response getResourceWithResponse(String path) {
        String url = getContextBaseUrl() + (!path.startsWith("/") ? "/" : "") + path;

        logger.info("Getting {}...", url);

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        return resource.request(MediaType.TEXT_PLAIN).header(HEADER_NAME_USER_AGENT, HEADER_VALUE_MAVEN).get();
    }

    @Override
    public void delete(String storageId,
                       String repositoryId,
                       String path)
            throws ArtifactOperationException {
        delete(storageId, repositoryId, path, false);
    }

    @Override
    public void delete(String storageId,
                       String repositoryId,
                       String path,
                       boolean force)
            throws ArtifactOperationException {
        String url = getContextBaseUrl() + "/api/browse/" + storageId + "/" + repositoryId + "/" + path +
                (force ? "?force=" + force : "");

        logger.info("Getting {}...", url);

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request().delete();

        handleFailures(response, "Failed to delete artifact!");
    }

    @Override
    public boolean pathExists(String path) {
        String url = escapeUrl(path);

        logger.info("Path to artifact: {}", url);

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.TEXT_PLAIN).header(HEADER_NAME_USER_AGENT, HEADER_VALUE_MAVEN).get();
        try {
            return response.getStatus() == HttpStatus.SC_OK;
        } finally {
            response.close();
        }
    }

    public void handleFailures(Response response,
                               String message)
            throws ArtifactOperationException, AuthenticationServiceException {

        int status = response.getStatus();

        if (status == SC_UNAUTHORIZED || status == SC_FORBIDDEN) {
            // TODO Handle authentication exceptions in a right way
            throw new AuthenticationServiceException(message +
                    "\nUser is unauthorized to execute that operation. " +
                    "Check assigned roles and privileges.");
        } else if (status != 200) {
            StringBuilder messageBuilder = new StringBuilder();
            messageBuilder.append("\n ERROR ").append(status).append(" ").append(message).append("\n");
            Object entity = response.getEntity();
            if (entity != null) {
                messageBuilder.append(entity.toString());
            }
            logger.error(messageBuilder.toString());
        }
    }

    public WebTarget setupAuthentication(WebTarget target) {
        if (username != null && password != null) {
            logger.trace("[setupAuthentication] {}", username);
            target.register(CustomAuthenticationFeature.create(username, password));
            return target;
        } else {
            throw new ServerErrorException("Unable to setup authentication", Response.Status.INTERNAL_SERVER_ERROR);
        }
    }

    public String getProtocol() {
        return protocol;
    }

    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }


    @Override
    public String getContextBaseUrl() {
        if (contextBaseUrl == null) {
            contextBaseUrl = protocol + "://" + host + ":" + port;
        }

        return contextBaseUrl;
    }

    public void setContextBaseUrl(String contextBaseUrl) {
        this.contextBaseUrl = contextBaseUrl;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

}
