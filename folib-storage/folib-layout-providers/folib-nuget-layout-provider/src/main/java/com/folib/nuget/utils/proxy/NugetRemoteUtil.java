package com.folib.nuget.utils.proxy;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.cache.NugetRemoteCache;
import com.folib.nugetv3.utils.JsonUtil;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import jakarta.xml.bind.JAXBContext;
import jakarta.xml.bind.Unmarshaller;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;


import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;



@Slf4j
public class NugetRemoteUtil {
    public static <T> T fetchFromRemote(@NonNull String remoteUrl, Class<T> clazz, FetchType type) throws Exception {
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet httpGet = new HttpGet(remoteUrl);
            CloseableHttpResponse response = httpClient.execute(httpGet);
            if (response.getStatusLine().getStatusCode() != HttpStatus.OK.value()) {
                log.error("Failed to fetch from proxy. Status code: {}", response.getStatusLine().getStatusCode());
                throw new RuntimeException("Failed to fetch from proxy");
            }
            HttpEntity entity = response.getEntity();
            String result = EntityUtils.toString(entity);
            return deserialize(result, clazz, type);
        } catch (Exception e) {
            log.error("Error fetching Json from proxy: {}", e.getMessage());
            throw e;
        }
    }

    public static <T> T fetchAndCache(String remoteUrl, Class<T> clazz, @NonNull RepositoryPath localPath, FetchType type) throws Exception {
        if (inCache(localPath)) {
            return readFromCache(localPath, clazz, type);
        }
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet httpGet = new HttpGet(remoteUrl);
            CloseableHttpResponse response = httpClient.execute(httpGet);
            if (response.getStatusLine().getStatusCode() != HttpStatus.OK.value()) {
                log.error("Failed to fetch NuGet service index from proxy. Status code: {}", response.getStatusLine().getStatusCode());
                throw new RuntimeException("Failed to fetch NuGet service index from proxy");
            }
            HttpEntity entity = response.getEntity();
            try (InputStream is = new BufferedInputStream(entity.getContent())) {
                // Cache the result
                cache(localPath, is);
            } catch (Exception e) {
                log.error("Error caching NuGet service index: {}", e.getMessage());
                throw e;
            }
            return readFromCache(localPath, clazz, type);
        } catch (Exception e) {
            log.error("Error fetching NuGet service index from proxy: {}", e.getMessage());
            throw e;
        }
    }

    public static void fetchAndCachePackage(@NonNull Repository repository, @NonNull String remoteUrl, @NonNull RepositoryPath localPath) throws Exception {
        if (inCache(localPath)) {
            log.info("Using cached data for {}", localPath);
            return;
        }
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet httpGet = new HttpGet(remoteUrl);
            CloseableHttpResponse response = httpClient.execute(httpGet);
            if (response.getStatusLine().getStatusCode() != HttpStatus.OK.value()) {
                log.error("Failed to fetch from proxy. Status code: {}", response.getStatusLine().getStatusCode());
                throw new RuntimeException("Failed to fetch from proxy");
            }
            HttpEntity entity = response.getEntity();
            try (InputStream is = new BufferedInputStream(entity.getContent())) {
                cache(localPath, is);
            } catch (Exception e) {
                log.error("Error caching data: {}", e.getMessage());
                throw e;
            }
        } catch (Exception e) {
            log.error("Error fetching data from proxy: {}", e.getMessage());
            throw e;
        }
    }

    public static void fetchAndCachePdb(@NonNull String remoteUrl, @NonNull RepositoryPath localPath, HttpHeaders httpHeaders) throws Exception {
        if (inCache(localPath)) {
            log.info("Using cached data for {}", localPath);
            return;
        }
        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpGet httpGet = new HttpGet(remoteUrl);
            // Set headers if provided
            if (httpHeaders != null) {
                httpHeaders.forEach((key, values) -> {
                    if (!"Host".equalsIgnoreCase(key)) {
                        values.forEach(value -> httpGet.addHeader(key, value));
                    }
                });
            }
            if (remoteUrl.contains("symbols.nuget.org")) {
                httpGet.addHeader("Host", "symbols.nuget.org");
            }
            CloseableHttpResponse response = httpClient.execute(httpGet);
            if (response.getStatusLine().getStatusCode() != HttpStatus.OK.value()) {
                log.error("Failed to fetch from proxy. Status code: {}", response.getStatusLine().getStatusCode());
                throw new RuntimeException("Failed to fetch from proxy");
            }
            HttpEntity entity = response.getEntity();
            try (InputStream is = new BufferedInputStream(entity.getContent())) {
                cache(localPath, is);
            } catch (Exception e) {
                log.error("Error caching data: {}", e.getMessage());
                throw e;
            }
        } catch (Exception e) {
            log.error("Error fetching data from proxy: {}", e.getMessage());
            throw e;
        }
    }

    public static boolean inCache(RepositoryPath repositoryPath) {
        NugetRemoteCache nugetRemoteCache = SpringUtil.getBean(NugetRemoteCache.class);
        return nugetRemoteCache.isActive(repositoryPath.toString()) &&
                Files.exists(repositoryPath);
    }


    private static void cache(@NonNull RepositoryPath path, InputStream inputStream) throws Exception {
        // save to file system
        if (!Files.exists(path.getParent())) {
            Files.createDirectories(path.getParent());
        }
        Files.copy(inputStream, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        // cache
        NugetRemoteCache nugetRemoteCache = SpringUtil.getBean(NugetRemoteCache.class);
        nugetRemoteCache.put(path.toString());
    }

    public static <T> T readFromCache(@NonNull RepositoryPath repositoryPath, Class<T> clazz, FetchType type) throws Exception {
        if (!Files.exists(repositoryPath)) {
            log.error("Repository path {} does not exist.", repositoryPath);
            return null;
        }
        String content = new String(Files.readAllBytes(repositoryPath), StandardCharsets.UTF_8);
        return deserialize(content, clazz, type);
    }


    private static <T> T deserialize(String content, Class<T> clazz, FetchType type) throws Exception {
        if (type == FetchType.XML) {
            JAXBContext jaxbContext = JAXBContext.newInstance(clazz);
            Unmarshaller unmarshaller = jaxbContext.createUnmarshaller();
            return clazz.cast(unmarshaller.unmarshal(new StringReader(content)));
        } else if (type == FetchType.JSON) {
            return JsonUtil.getInstance().readValue(content, clazz);
        } else if (type == FetchType.STRING) {
            return (T) content;
        } else {
            throw new IllegalArgumentException("Unsupported fetch type: " + type);
        }
    }


}
