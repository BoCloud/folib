package com.veadan.folib.services.impl;

import com.beust.jcommander.internal.Maps;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.services.ConanArtifactServer;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Map;

@Service
@Slf4j
public class ConanArtifactServerIpml implements ConanArtifactServer {

    @Autowired
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Override
    public ResponseEntity searchConanPackage(Repository repository, String query) {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();
        log.info("Requested /{}/{}/{}.", storageId, repositoryId, "v1/conans/search");
        //通过代理模式去获取返回的结果
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        Map<String, Object> map = null;
        try {
            map = repositoryProvider.searchConanPackage(repository, query);
        } catch (Exception e) {
            e.printStackTrace();
            return new ResponseEntity<>(Maps.newHashMap(), HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @Override
    public ResponseEntity downloadUrls(Repository repository, String name, String version, String user, String channel) {
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        Map<String, String> map = repositoryProvider.searchConanDownLoadUrl(repository, name, version, user, channel);
        return new ResponseEntity<>(map, HttpStatus.OK);
    }

    @Override
    public ResponseEntity searchConanPackageInfo(Repository repository, String name, String version, String user, String channel) {
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        Map<String, Object> map = null;
        try {
            map = repositoryProvider.searchConanPackageInfo(repository, name, version, user, channel);
        } catch (IOException e) {
            e.printStackTrace();
            new ResponseEntity<>("{}", HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return new ResponseEntity<>(map, HttpStatus.OK);
    }
}
