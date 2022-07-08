package com.veadan.folib.client;

import com.veadan.folib.yaml.CustomYAMLMapperFactory;
import com.veadan.folib.yaml.ObjectMapperSubtypes;
import com.veadan.folib.yaml.YAMLMapperFactory;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.configuration.MutableProxyConfiguration;
import com.veadan.folib.forms.configuration.RepositoryForm;
import com.veadan.folib.forms.configuration.StorageForm;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageData;
import com.veadan.folib.storage.repository.RepositoryData;

import javax.ws.rs.ServerErrorException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Set;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author mtodorov
 * @author veadan
 */
public class RestClient
        extends ArtifactClient
{

    private final YAMLMapper yamlMapper;

    private static final Logger logger = LoggerFactory.getLogger(RestClient.class);

    private static final MediaType YAML_MEDIA_TYPE = MediaType.valueOf(com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE);

    public RestClient(YAMLMapper yamlMapper)
    {
        this.yamlMapper = yamlMapper;
    }

    public static ArtifactClient getTestInstance()
    {
        return getTestInstance("deployer", "password");
    }

    public static RestClient getTestInstanceLoggedInAsAdmin()
    {
        return getTestInstance("admin", "password");
    }

    public static RestClient getTestInstance(String username,
                                             String password)
    {
        String host = System.getProperty("folib.host") != null ?
                      System.getProperty("folib.host") :
                      "localhost";

        int port = System.getProperty("folib.port") != null ?
                   Integer.parseInt(System.getProperty("folib.port")) :
                   48080;

        Set<Class<?>> objectMapperSubtypes = ObjectMapperSubtypes.INSTANCE.subtypes();
        logger.info("Found subtypes {} ", objectMapperSubtypes);

        YAMLMapperFactory yamlMapperFactory = new CustomYAMLMapperFactory();
        YAMLMapper yamlMapper = yamlMapperFactory.create(objectMapperSubtypes);
        yamlMapper.disable(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE);

        RestClient client = new RestClient(yamlMapper);
        client.setUsername(username);
        client.setPassword(password);
        client.setPort(port);
        client.setContextBaseUrl("http://" + host + ":" + client.getPort());

        return client;
    }

    public static void displayResponseError(Response response)
    {
        logger.error("Status code {}", response.getStatus());
        logger.error("Status info {}", response.getStatusInfo().getReasonPhrase());
        logger.error("Response message {}", response.readEntity(String.class));
        logger.error(response.toString());
    }

    public int setConfiguration(MutableConfiguration configuration)
            throws IOException
    {
        return setServerConfiguration(configuration, "/api/configuration/folib");
    }

    public MutableConfiguration getConfiguration()
            throws IOException
    {
        return getServerConfiguration("/api/configuration/folib");
    }

    public int setServerConfiguration(MutableConfiguration configuration,
                                      String path)
            throws IOException
    {
        String url = getContextBaseUrl() + path;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        try (final ByteArrayOutputStream baos = new ByteArrayOutputStream())
        {

            yamlMapper.writeValue(baos, configuration);

            Response response = resource.request(MediaType.TEXT_PLAIN_TYPE)
                                        .put(Entity.entity(baos.toString("UTF-8"), YAML_MEDIA_TYPE));

            return response.getStatus();
        }
    }

    public MutableConfiguration getServerConfiguration(String path)
            throws IOException
    {
        String url = getContextBaseUrl() + path;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        final Response response = resource.request(YAML_MEDIA_TYPE).get();

        MutableConfiguration configuration = null;
        if (response.getStatus() == 200)
        {
            final String yaml = response.readEntity(String.class);

            try (final ByteArrayInputStream bais = new ByteArrayInputStream(yaml.getBytes()))
            {
                configuration = yamlMapper.readValue(bais, MutableConfiguration.class);
            }
            catch (Exception ex)
            {
                logger.error("Exception occurred ", ex);
                throw ex;
            }
        }

        return configuration;
    }

    /**
     * Sets the listening port.
     *
     * @param port The port to listen on.
     * @return The response from the server.
     */
    public int setListeningPort(int port)
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/port/" + port;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.TEXT_PLAIN).put(Entity.entity(port, MediaType.TEXT_PLAIN));

        return response.getStatus();
    }

    /**
     * Get the port on which the server is listening.
     *
     * @return The port on which the server is listening.
     */
    public int getListeningPort()
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/port";

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        return resource.request(MediaType.TEXT_PLAIN).get(Integer.class);
    }

    /**
     * Sets the base URL of the server.
     *
     * @param baseUrl The base URL.
     * @return The response code.
     */
    public int setBaseUrl(String baseUrl)
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/baseUrl/" + baseUrl;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.TEXT_PLAIN).put(Entity.entity(baseUrl, MediaType.TEXT_PLAIN));

        return response.getStatus();
    }

    /**
     * Gets the base URL of the server.
     *
     * @return The response code.
     */
    public String getBaseUrl()
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/baseUrl";

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        return resource.request(MediaType.TEXT_PLAIN).get(String.class);
    }

    public int setProxyConfiguration(MutableProxyConfiguration proxyConfiguration)
            throws IOException
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/proxy-configuration";

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        try (final ByteArrayOutputStream baos = new ByteArrayOutputStream())
        {
            yamlMapper.writeValue(baos, proxyConfiguration);

            Response response = resource.request(MediaType.TEXT_PLAIN_TYPE)
                                        .put(Entity.entity(baos.toString("UTF-8"), YAML_MEDIA_TYPE));

            return response.getStatus();
        }
    }

    public MutableProxyConfiguration getProxyConfiguration(String storageId,
                                                           String repositoryId)
            throws IOException
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/proxy-configuration" +
                     (storageId != null && repositoryId != null ?
                      "?storageId=" + storageId + "&repositoryId=" + repositoryId : "");

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        final Response response = resource.request(YAML_MEDIA_TYPE).get();

        @SuppressWarnings("UnnecessaryLocalVariable")
        MutableProxyConfiguration proxyConfiguration;
        if (response.getStatus() == 200)
        {
            final String yaml = response.readEntity(String.class);

            try (final ByteArrayInputStream bais = new ByteArrayInputStream(yaml.getBytes()))
            {
                proxyConfiguration = yamlMapper.readValue(bais, MutableProxyConfiguration.class);
            }
        }
        else
        {
            proxyConfiguration = new MutableProxyConfiguration();
        }

        return proxyConfiguration;
    }

    /**
     * Creates a new storage.
     *
     * @param storage The storage object to create.
     * @return The response code.
     */
    public int addStorage(StorageForm storage)
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages";

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.APPLICATION_JSON_TYPE)
                                    .put(Entity.entity(storage, MediaType.APPLICATION_JSON_TYPE));

        return response.getStatus();
    }

    /**
     * Looks up a storage by it's ID.
     *
     * @param storageId
     * @return
     * @throws IOException
     */
    public Storage getStorage(String storageId)
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        final Response response = resource.request(MediaType.APPLICATION_JSON).get();

        StorageData storage = null;
        if (response.getStatus() == 200)
        {
            storage = response.readEntity(StorageData.class);
        }
        else
        {
            displayResponseError(response);
        }

        return storage;
    }

    /**
     * Deletes a storage.
     *
     * @param storageId The storage to delete.
     * @return
     */
    public int deleteStorage(String storageId,
                             boolean force)
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId +
                     (force ? "?force=true" : "");

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request().delete();

        return response.getStatus();
    }

    public int addRepository(String storageId,
                             RepositoryForm repositoryForm)
    {
        if (repositoryForm == null)
        {
            logger.error("Unable to add non-existing repository.");
            throw new ServerErrorException("Unable to add non-existing repository.",
                                           Response.Status.INTERNAL_SERVER_ERROR);
        }

        WebTarget resource;

        if (storageId == null)
        {
            logger.error("Storage associated with repo is null.");
            throw new ServerErrorException("Storage associated with repo is null.",
                                           Response.Status.INTERNAL_SERVER_ERROR);
        }

        try
        {
            String url = getContextBaseUrl() + "/api/configuration/folib/storages/" +
                         storageId + "/" + repositoryForm.getId();

            logger.debug("Sending request to create repository " + url);

            resource = getClientInstance().target(url);
        }
        catch (RuntimeException e)
        {
            logger.error("Unable to create web resource.", e);
            throw new ServerErrorException(Response.Status.INTERNAL_SERVER_ERROR);
        }

        setupAuthentication(resource);

        Response response = resource.request(MediaType.APPLICATION_JSON)
                                    .put(Entity.entity(repositoryForm, MediaType.APPLICATION_JSON));

        return response.getStatus();
    }

    /**
     * Looks up a repository by it's ID.
     *
     * @param storageId
     * @param repositoryId
     * @return
     * @throws java.io.IOException
     */
    public RepositoryData getRepository(String storageId,
                                        String repositoryId)
    {
        String url = getContextBaseUrl() + "/api/configuration/folib/storages/" + storageId + "/" + repositoryId;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        final Response response = resource.request(MediaType.APPLICATION_JSON).get();

        RepositoryData repository = null;
        if (response.getStatus() == 200)
        {
            repository = response.readEntity(RepositoryData.class);
        }
        else
        {
            displayResponseError(response);
        }

        return repository;
    }

    /**
     * Deletes a repository.
     *
     * @param storageId    The storage in which the repository to delete is under.
     * @param repositoryId The repository to delete.
     * @return
     */
    public int deleteRepository(String storageId,
                                String repositoryId,
                                boolean force)
    {
        String url = getContextBaseUrl() +
                     "/api/configuration/folib/storages/" + storageId + "/" + repositoryId +
                     (force ? "?force=true" : "");

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request().delete();

        return response.getStatus();
    }

    public String search(String query,
                         MediaType mediaType)
            throws UnsupportedEncodingException
    {
        return search(null, query, mediaType);
    }

    public String search(String repositoryId,
                         String query,
                         MediaType mediaType)
            throws UnsupportedEncodingException
    {
        String url = getContextBaseUrl() + "/api/search?" +
                     (repositoryId != null ? "repositoryId=" + URLEncoder.encode(repositoryId, "UTF-8") : "") +
                     "&q=" + URLEncoder.encode(query, "UTF-8");

        WebTarget webResource = getClientInstance().target(url);
        setupAuthentication(webResource);

        final Response response = webResource.request(mediaType).get();

        //noinspection UnnecessaryLocalVariable
        final String asText = response.readEntity(String.class);

        return asText;
    }

    public int rebuildMetadata(String storageId,
                               String repositoryId,
                               String basePath)
    {
        String url = getContextBaseUrl() + "/api/maven/metadata/" + storageId + "/" + repositoryId + "/" +
                     (basePath != null ? basePath : "");

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.TEXT_PLAIN).post(Entity.entity("Rebuild",
                                                                                      MediaType.APPLICATION_XML));

        return response.getStatus();
    }

    public int removeVersionFromMetadata(String storageId,
                                         String repositoryId,
                                         String artifactPath,
                                         String version,
                                         String classifier,
                                         String metadataType)
    {
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

    public void copy(String path,
                     String srcStorageId,
                     String srcRepositoryId,
                     String destStorageId,
                     String destRepositoryId)
    {
        @SuppressWarnings("ConstantConditions")
        String url = getContextBaseUrl() + "/storages/copy/" + path +
                     "?srcStorageId=" + srcStorageId +
                     "&srcRepositoryId=" + srcRepositoryId +
                     "&destStorageId=" + destStorageId +
                     "&destRepositoryId=" + destRepositoryId;

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        resource.request(MediaType.TEXT_PLAIN).post(Entity.entity("Copy", MediaType.TEXT_PLAIN));
    }

    public String generateUserSecurityToken()
    {
        String url = String.format("%s/api/users/%s/generate-security-token", getContextBaseUrl(), getUsername());

        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.TEXT_PLAIN).get();
        if (response.getStatus() != 200)
        {
            displayResponseError(response);
            throw new ServerErrorException(response.getEntity() + " | Unable to get Security Token",
                                           response.getStatus());
        }
        else
        {
            return response.readEntity(String.class);
        }
    }

    public String greet()
    {
        String url = getContextBaseUrl() + "/storages/greet";
        WebTarget resource = getClientInstance().target(url);
        setupAuthentication(resource);

        Response response = resource.request(MediaType.TEXT_PLAIN).get();
        if (response.getStatus() != 200)
        {
            displayResponseError(response);
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                                           Response.Status.INTERNAL_SERVER_ERROR);
        }
        else
        {
            return response.getEntity().toString();
        }
    }

    public WebTarget prepareTarget(String arg)
    {
        return setupAuthentication(prepareUnauthenticatedTarget(arg));
    }

    public WebTarget prepareUnauthenticatedTarget(String arg)
    {
        String url = getContextBaseUrl() + arg;

        logger.debug("Prepare target URL {}", url);

        return getClientInstance().target(url);
    }

    public WebTarget prepareTarget(String arg,
                                   String username,
                                   String password)
    {
        this.username = username;
        this.password = password;

        return prepareTarget(arg);
    }

}
