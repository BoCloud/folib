package com.folib.nuget.utils;

import cn.hutool.extra.spring.SpringUtil;
import com.folib.configuration.ConfigurationManager;
import com.folib.nugetv3.registration.NugetV3RegistrationUtil;
import com.folib.storage.repository.Repository;
import com.folib.utils.PathUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;



public class NuGetUrlBuilder {
    public static String registration(String v3RegistrationUrl, String packageId) {
        Objects.requireNonNull(v3RegistrationUrl);
        v3RegistrationUrl = PathUtils.trimTrailingSlashes(v3RegistrationUrl);
        return v3RegistrationUrl + "/" + packageId.toLowerCase() + "/index.json";
    }

    public static String registrationPage(String v3RegistrationBaseUrl, String packageId, String lower, String upper) {
        Objects.requireNonNull(v3RegistrationBaseUrl);
        Objects.requireNonNull(lower);
        Objects.requireNonNull(upper);
        v3RegistrationBaseUrl = PathUtils.trimTrailingSlashes(v3RegistrationBaseUrl);
        return v3RegistrationBaseUrl + "/" + packageId.toLowerCase() + "/page/" + NugetV3RegistrationUtil.trimSemVer2MetaData(lower).toLowerCase() + "/" + NugetV3RegistrationUtil.trimSemVer2MetaData(upper).toLowerCase() + ".json";
    }

    public static String registrationSpecificVersion(String v3RegistrationBaseUrl, String packageId, String version) {
        Objects.requireNonNull(v3RegistrationBaseUrl);
        Objects.requireNonNull(version);
        v3RegistrationBaseUrl = PathUtils.trimTrailingSlashes(v3RegistrationBaseUrl);
        return v3RegistrationBaseUrl + "/" + packageId.toLowerCase() + "/" + NugetV3RegistrationUtil.trimSemVer2MetaData(version) + ".json";
    }

    public static String packageContent(String v2BaseUrl, String packageId, String version) {
        Objects.requireNonNull(v2BaseUrl);
        Objects.requireNonNull(version);
        v2BaseUrl = PathUtils.trimTrailingSlashes(v2BaseUrl);
        return v2BaseUrl + "/download/nupkg/" + packageId.toLowerCase() + "/" + version.toLowerCase();
    }

    public static String getNugetRepositoryUrl(Repository repository) {
        ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        String separator = "/";
        String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), separator);
        return baseUrl + String.format("/storages/%s/%s", storageId, repositoryId);
    }

    public static String getNugetV2BaseUrl(Repository repository) {
        String baseUrl = getNugetRepositoryUrl(repository);
        return baseUrl + "/api/v2";
    }

    public static String getNugetV3RegistrationBaseUrl(Repository repository, boolean isSemVer2Endpoint) {
        String baseUrl = getNugetRepositoryUrl(repository);
        if (isSemVer2Endpoint) {
            return baseUrl + "/api/v3/registration-semver2";
        } else {
            return baseUrl + "/api/v3/registration";
        }
    }

    public static String getNugetV2PackageIdUrl(String v2BaseUrl, String packageId, String version) {
        return String.format("%s/Packages(Id='%s',Version='%s')", v2BaseUrl, packageId, version);
    }

}
