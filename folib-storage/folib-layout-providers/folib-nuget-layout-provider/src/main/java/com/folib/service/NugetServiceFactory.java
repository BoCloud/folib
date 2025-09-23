package com.folib.service;

import com.folib.storage.repository.Repository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;




@Service
public class NugetServiceFactory {
    Map<String, NugetV2Service> nugetV2ServiceMap;
    Map<String, NugetV3Service> nugetV3ServiceMap;

    @Autowired
    public NugetServiceFactory(List<NugetV2Service> nugetV2Services, List<NugetV3Service> nugetV3Services) {
        nugetV2ServiceMap = nugetV2Services.stream()
                .collect(Collectors.toMap(NugetV2Service::getType, Function.identity()));
        nugetV3ServiceMap = nugetV3Services.stream()
                .collect(Collectors.toMap(NugetV3Service::getType, Function.identity()));
    }

    public NugetV2Service getNugetV2Service(Repository repository) {
        String type = "";
        if (repository == null || repository.isHostedRepository()) {
            type = "HOST";
        } else if (repository.isProxyRepository()) {
            type = "PROXY";
        } else if (repository.isGroupRepository()) {
            type = "GROUP";
        } else {
            throw new IllegalArgumentException("Unsupported repository type: " + repository.getType());
        }
        NugetV2Service nugetV2Service = nugetV2ServiceMap.get(type);
        if (nugetV2Service == null) {
            throw new IllegalArgumentException("No NugetV2Service found for type: " + type);
        }
        return nugetV2Service;
    }

    public NugetV3Service getNugetV3Service(Repository repository) {
        String type = "";
        if (repository == null || repository.isHostedRepository()) {
            type = "HOST";
        } else if (repository.isProxyRepository()) {
            type = "PROXY";
        } else if (repository.isGroupRepository()) {
            type = "GROUP";
        } else {
            throw new IllegalArgumentException("Unsupported repository type: " + repository.getType());
        }
        NugetV3Service nugetV3Service = nugetV3ServiceMap.get(type);
        if (nugetV3Service == null) {
            throw new IllegalArgumentException("No NugetV3Service found for type: " + type);
        }
        return nugetV3Service;
    }
}
