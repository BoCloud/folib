package com.veadan.folib.client;

import com.alibaba.fastjson.JSONArray;
import com.veadan.folib.forms.RepositoryForm;
import com.veadan.folib.forms.StorageForm;
import com.veadan.folib.vo.Repository;
import com.veadan.folib.vo.Storage;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ws.rs.ServerErrorException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.List;

/**
 * folib rest client
 *
 * @author veadan
 */
public class RestClient extends ArtifactClient {

    private static final Logger logger = LoggerFactory.getLogger(RestClient.class);

    public RestClient() {

    }

    /**
     * 获取客户端实例
     *
     * @param username 用户名
     * @param password 密码
     * @return 客户端实例
     */
    public static RestClient getRestClientInstance(String username, String password) {
        String host = System.getProperty("folib.host") != null ?
                System.getProperty("folib.host") :
                "localhost";
        int port = System.getProperty("folib.port") != null ?
                Integer.parseInt(System.getProperty("folib.port")) :
                38080;
        RestClient client = new RestClient();
        client.setUsername(username);
        client.setPassword(password);
        client.setPort(port);
        client.setContextBaseUrl("http://" + host + ":" + client.getPort());
        return client;
    }

    /**
     * 获取客户端实例
     *
     * @param baseUrl  地址
     * @param username 用户名
     * @param password 密码
     * @return 客户端实例
     */
    public static RestClient getRestClientInstance(String baseUrl, String username, String password) {
        int port = Integer.parseInt(baseUrl.substring(baseUrl.lastIndexOf(":") + 1));
        RestClient client = new RestClient();
        client.setUsername(username);
        client.setPassword(password);
        client.setPort(port);
        client.setContextBaseUrl(baseUrl);
        return client;
    }

    /**
     * 返回错误信息
     *
     * @param response response
     */
    public static void displayResponseError(Response response) {
        logger.error("Status code {}", response.getStatus());
        logger.error("Status info {}", response.getStatusInfo().getReasonPhrase());
        logger.error("Response message {}", response.readEntity(String.class));
        logger.error(response.toString());
    }

    /**
     * 设置监听端口
     *
     * @param port 要监听的端口
     * @return 响应状态码
     */
    public int setListeningPort(int port) {
        String url = getContextBaseUrl() + "/api/configuration/folib/port/" + port;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).put(Entity.entity(port, MediaType.TEXT_PLAIN));
        return response.getStatus();
    }


    /**
     * 获取正在监听的端口
     *
     * @return 正在监听的端口
     */
    public int getListeningPort() {
        String url = getContextBaseUrl() + "/api/configuration/folib/port";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        return resource.request(MediaType.TEXT_PLAIN).get(Integer.class);
    }

    /**
     * 设置服务基础路径
     *
     * @param baseUrl 服务基础路径
     * @return 响应状态码
     */
    public int setBaseUrl(String baseUrl) {
        String url = getContextBaseUrl() + "/api/configuration/folib/baseUrl/" + baseUrl;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).put(Entity.entity(baseUrl, MediaType.TEXT_PLAIN));
        return response.getStatus();
    }

    /**
     * 获取服务基础路径
     *
     * @return 服务基础路径
     */
    public String getBaseUrl() {
        String url = getContextBaseUrl() + "/api/configuration/folib/baseUrl";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        return resource.request(MediaType.TEXT_PLAIN).get(String.class);
    }

    /**
     * 创建存储空间
     *
     * @param storage 存储空间信息
     * @return 响应状态码
     */
    public int addStorage(StorageForm storage) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON_TYPE)
                .put(Entity.entity(storage, MediaType.APPLICATION_JSON_TYPE));
        return response.getStatus();
    }

    /**
     * 按照存储空间名称查询存储空间信息
     *
     * @param storageId 存储空间名称
     * @return 存储空间信息
     */
    public Storage getStorage(String storageId) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        final Response response = resource.request(MediaType.APPLICATION_JSON).get();
        Storage storage = null;
        if (response.getStatus() == HttpStatus.SC_OK) {
            storage = response.readEntity(Storage.class);
        } else {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        return storage;
    }

    /**
     * 删除存储空间
     *
     * @param storageId 存储空间名称
     * @param force     强制删除 true 是 false 否
     * @return 响应状态码
     */
    public int deleteStorage(String storageId, boolean force) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId +
                (force ? "?force=true" : "");
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().delete();
        return response.getStatus();
    }

    /**
     * 创建仓库
     *
     * @param storageId      存储空间名称
     * @param repositoryForm 仓库信息
     * @return 响应状态码
     */
    public int addRepository(String storageId, RepositoryForm repositoryForm) {
        if (repositoryForm == null) {
            logger.error("Unable to add non-existing repository.");
            throw new ServerErrorException("Unable to add non-existing repository.",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        WebTarget resource;
        if (storageId == null) {
            logger.error("Storage associated with repo is null.");
            throw new ServerErrorException("Storage associated with repo is null.",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        try {
            String url = getContextBaseUrl() + "/api/configuration/folib/storages/" +
                    storageId + "/" + repositoryForm.getId();
            logger.debug("Sending request to create repository " + url);
            resource = getClientInstance().target(url);
        } catch (RuntimeException e) {
            logger.error("Unable to create web resource.", e);
            throw new ServerErrorException(Response.Status.INTERNAL_SERVER_ERROR);
        }
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON)
                .put(Entity.entity(repositoryForm, MediaType.APPLICATION_JSON));
        return response.getStatus();
    }

    /**
     * 按照存储空间名称和仓库名称查询存储信息
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @return 存储信息
     */
    public Repository getRepository(String storageId, String repositoryId) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId + "/" + repositoryId;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        final Response response = resource.request(MediaType.APPLICATION_JSON).get();
        Repository repository = null;
        if (response.getStatus() == HttpStatus.SC_OK) {
            repository = response.readEntity(Repository.class);
        } else {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        }
        return repository;
    }

    /**
     * 删除仓库
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param force        强制删除 true 是 false 否
     * @return 响应状态码
     */
    public int deleteRepository(String storageId, String repositoryId, boolean force) {
        String url = getContextBaseUrl() +
                "/api/configuration/folib/storages/" + storageId + "/" + repositoryId +
                ("?force=" + force);
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().accept(MediaType.WILDCARD).delete();
        return response.getStatus();
    }

    /**
     * 搜索制品
     *
     * @param query     关键词
     * @param mediaType mediaType
     * @return 结果
     * @throws UnsupportedEncodingException unsupportedEncodingException
     */
    public String search(String query, MediaType mediaType) throws UnsupportedEncodingException {
        return search(null, query, mediaType);
    }

    /**
     * 搜索制品
     *
     * @param repositoryId 仓库名称
     * @param query        关键词
     * @param mediaType    mediaType
     * @return 结果
     * @throws UnsupportedEncodingException unsupportedEncodingException
     */
    public String search(String repositoryId, String query, MediaType mediaType) throws UnsupportedEncodingException {
        String url = getContextBaseUrl() + "/api/search?" +
                (repositoryId != null ? "repositoryId=" + URLEncoder.encode(repositoryId, "UTF-8") : "") +
                "&q=" + URLEncoder.encode(query, "UTF-8");

        WebTarget webResource = getClientInstance().target(url);
        setupAuthentication(webResource);
        final Response response = webResource.request(mediaType).get();
        final String asText = response.readEntity(String.class);
        return asText;
    }

    /**
     * 重建maven metadata
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param basePath     路径
     * @return 响应状态码
     */
    public int rebuildMetadata(String storageId, String repositoryId, String basePath) {
        String url = getContextBaseUrl() + "/api/maven/metadata/" + storageId + "/" + repositoryId + "/" +
                (basePath != null ? basePath : "");
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).post(Entity.entity("Rebuild",
                MediaType.APPLICATION_XML));
        return response.getStatus();
    }

    /**
     * 从maven metadata 中删除版本信息
     *
     * @param storageId    存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @param version      版本
     * @param classifier   classifier
     * @param metadataType metadataType
     * @return 响应状态码
     */
    public int removeVersionFromMetadata(String storageId, String repositoryId, String artifactPath, String version, String classifier, String metadataType) {
        String url = getContextBaseUrl() + "/api/maven/metadata/" +
                storageId + "/" + repositoryId + "/" +
                (artifactPath != null ? artifactPath : "") +
                "?version=" + version + (classifier != null ? "&classifier=" + classifier : "") +
                "&metadataType=" + metadataType;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request().delete();
        return response.getStatus();
    }

    /**
     * 制品复制
     *
     * @param path             路径
     * @param srcStorageId     源存储空间名称
     * @param srcRepositoryId  源仓库名称
     * @param destStorageId    目标存储空间名称
     * @param destRepositoryId 目标仓库名称
     */
    public void copy(String path, String srcStorageId, String srcRepositoryId, String destStorageId, String destRepositoryId) {
        String url = getContextBaseUrl() + "/storages/copy/" + path +
                "?srcStorageId=" + srcStorageId +
                "&srcRepositoryId=" + srcRepositoryId +
                "&destStorageId=" + destStorageId +
                "&destRepositoryId=" + destRepositoryId;
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        resource.request(MediaType.TEXT_PLAIN).post(Entity.entity("Copy", MediaType.TEXT_PLAIN));
    }

    /**
     * 生成token
     *
     * @return token
     */
    public String generateUserSecurityToken() {
        String url = String.format("%s/api/users/%s/generate-security-token", getContextBaseUrl(), getUsername());
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.TEXT_PLAIN).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getEntity() + " | Unable to get Security Token",
                    response.getStatus());
        } else {
            return response.readEntity(String.class);
        }
    }


    /**
     * 按照存储空间名称和仓库类型查询存储空间下的仓库列表
     *
     * @param storageId      存储空间名称
     * @param repositoryType 仓库类型 hosted 本地库 proxy 代理库 group 组合库 all 所有仓库类型
     * @return 存储空间下的仓库列表
     */
    public List<Repository> getRepositories(String storageId, String repositoryType) {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/repositories/%s/%s";
        url = String.format(url, storageId, repositoryType);
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);
        Response response = resource.request(MediaType.APPLICATION_JSON).get();
        if (response.getStatus() != HttpStatus.SC_OK) {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String res = response.readEntity(String.class);
            if (StringUtils.isNotBlank(res)) {
                return JSONArray.parseArray(res, Repository.class);
            } else {
                return Collections.emptyList();
            }
        }
    }

    public WebTarget prepareTarget(String arg) {
        return setupAuthentication(prepareUnauthenticatedTarget(arg));
    }

    public WebTarget prepareUnauthenticatedTarget(String arg) {
        String url = getContextBaseUrl() + arg;
        logger.debug("Prepare target URL {}", url);
        return getClientInstance().target(url);
    }

    public WebTarget prepareTarget(String arg, String username, String password) {
        this.username = username;
        this.password = password;
        return prepareTarget(arg);
    }

}
