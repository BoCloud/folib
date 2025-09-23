package com.folib.service.v2;

import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.cache.NugetPackageCache;
import com.folib.cache.NugetSearchCountCache;
import com.folib.nuget.filter.NugetSearchRequest;
import com.folib.nuget.indexer.symbols.NugetSymbolsIndexer;
import com.folib.nuget.odata.feed.Entry;
import com.folib.nuget.odata.feed.Feed;
import com.folib.nuget.search.NugetFeedUtil;
import com.folib.nuget.search.NugetV2HostSearchUtil;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.NugetV2Service;
import com.folib.storage.repository.Repository;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;

import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;



@Component
@Slf4j
public class NugetV2HostService implements NugetV2Service {
    private final NugetPackageCache registrationResultPageItemCache;

    private final NugetV2HostSearchUtil nugetHostSearchUtil;

    private final RepositoryPathResolver repositoryPathResolver;

    private final NugetSearchCountCache nugetSearchCountCache;

    @Autowired
    public NugetV2HostService(NugetPackageCache registrationResultPageItemCache, NugetV2HostSearchUtil nugetHostSearchUtil, RepositoryPathResolver repositoryPathResolver, NugetSearchCountCache nugetSearchCountCache) {
        this.registrationResultPageItemCache = registrationResultPageItemCache;
        this.nugetHostSearchUtil = nugetHostSearchUtil;
        this.repositoryPathResolver = repositoryPathResolver;
        this.nugetSearchCountCache = nugetSearchCountCache;
    }


    @Override
    public String getType() {
        return "HOST";
    }

    @Override
    public Feed search(NugetSearchRequest searchRequest) throws Exception {
        Map<String, Map<String, Entry>> searchedPackages = nugetHostSearchUtil.searchHostItems(searchRequest);
        String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(searchRequest.getRepository());
        return NugetFeedUtil.buildFeed(searchedPackages, v2BaseUrl, "Search");
    }

    @Override
    public Feed findPackageById(Repository repository, String packageId, boolean isSemVer2Endpoint) throws Exception {
        List<Entry> allPackages = registrationResultPageItemCache.getFeedEntries(repository, packageId);
        String v2BaseUrl = NuGetUrlBuilder.getNugetV2BaseUrl(repository);
        return NugetFeedUtil.buildFeed(allPackages, v2BaseUrl, "FindPackagesById");
    }

    @Override
    public Entry packageEntry(Repository repository, String packageId, String version) throws Exception {
        Entry entry = registrationResultPageItemCache.getFeedEntry(repository, packageId, version);
        if (entry == null) {
            log.warn("Package {} with version {} not found in repository {}", packageId, version, repository.getId());
            return null;
        }
        return entry;
    }

    @Override
    public void provideDownloadNupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception {
        NugetCoordinates coordinates = new NugetCoordinates(packageId, version);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        if (repository == null || !Files.exists(repositoryPath)) {
            throw new Exception("Nupkg file not found: " + repositoryPath.getPath());
        }

        response.setContentType("application/octet-stream");
        response.setHeader("Content-Disposition", "attachment; filename=\"" + packageId + "." + version + ".nupkg\"");
        response.setHeader("Content-Length", String.valueOf(Files.size(repositoryPath)));

        // 将文件内容复制到响应输出流
        try (InputStream inputStream = Files.newInputStream(repositoryPath);
             OutputStream outputStream = response.getOutputStream()) {
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            outputStream.flush();
        }
    }

    @Override
    public void provideDownloadSnupkg(Repository repository, String packageId, String version, HttpServletResponse response) throws Exception {
        NugetCoordinates coordinates = new NugetCoordinates(packageId, version, NugetCoordinates.SYMBOL_EXTENSION);
        RepositoryPath artifactPath = repositoryPathResolver.resolve(repository, coordinates);
        if (artifactPath == null || !Files.exists(artifactPath)) {
            throw new Exception("Snupkg file" + packageId + "." + version + ".snupkg not found");
        }
        response.setContentType("application/octet-stream");
        response.setHeader("Content-Disposition", "attachment; filename=\"" + packageId + "." + version + ".snupkg\"");
        response.setHeader("Content-Length", String.valueOf(Files.size(artifactPath)));
        // 将文件内容复制到响应输出流
        try (InputStream inputStream = Files.newInputStream(artifactPath);
             OutputStream outputStream = response.getOutputStream()) {
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            outputStream.flush();
        }
    }

    @Override
    public int searchCount(NugetSearchRequest searchRequest) {
        try {
            if (nugetSearchCountCache.containsV2(searchRequest)) {
                return nugetSearchCountCache.getV2(searchRequest);
            }
            Feed feed = this.search(searchRequest);
            int count = feed.getEntries().size();
            nugetSearchCountCache.putV2(searchRequest, count);
            return count;
        } catch (Exception e) {
            nugetSearchCountCache.putV2(searchRequest, 0);
            return 0;
        }
    }

    @Override
    public void provideDownloadSymbolPdb(Repository repository, String packageId, String guid, HttpServletResponse response, HttpHeaders httpHeaders) throws Exception {
        String subPath = NugetSymbolsIndexer.getSymbolFilePathInCache(packageId, guid);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, subPath);
        if (!Files.exists(repositoryPath)) {
            throw new Exception("Symbol PDB file not found: " + repositoryPath.getPath());
        }
        response.setContentType("application/octet-stream");
        response.setHeader("Content-Disposition", "attachment; filename=\"" + packageId + ".pdb\"");
        response.setHeader("Content-Length", String.valueOf(Files.size(repositoryPath)));
        // 将文件内容复制到响应输出流
        try (InputStream inputStream = Files.newInputStream(repositoryPath);
             OutputStream outputStream = response.getOutputStream()) {
            byte[] buffer = new byte[4096];
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            outputStream.flush();
        }
    }
}
