package com.folib.nugetv3.model.index;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;


public class DefaultResources {
    public static Resource PACKAGE_PUBLISH_2_0_0 = Resource.builder()
            .type("PackagePublish/2.0.0")
            .id("@/api/v2/package")
            .build();

    public static Resource SYMBOL_PACKAGE_PUBLISH_4_9_0 = Resource.builder()
            .type("SymbolPackagePublish/4.9.0")
            .id("@/api/v2/symbols")
            .build();

    public static Resource REGISTRATION_BASE_URL = Resource.builder()
            .type("RegistrationBaseUrl")
            .id("@/api/v3/registration/")
            .build();

    public static Resource REGISTRATION_BASE_URL_3_0_0_BETA = Resource.builder()
            .type("RegistrationsBaseUrl/3.0.0-beta")
            .id("@/api/v3/registration/")
            .build();

    public static Resource REGISTRATION_BASE_URL_3_0_0_RC = Resource.builder()
            .type("RegistrationsBaseUrl/3.0.0-rc")
            .id("@/api/v3/registration/")
            .build();

    public static Resource REGISTRATION_BASE_URL_3_4_0 = Resource.builder()
            .type("RegistrationsBaseUrl/3.4.0")
            .id("@/api/v3/registration/")
            .build();

    public static Resource REGISTRATION_BASE_URL_3_6_0 = Resource.builder()
            .type("RegistrationsBaseUrl/3.6.0")
            .id("@/api/v3/registration-semver2/")
            .build();

    public static Resource REGISTRATION_BASE_URL_VERSIONED = Resource.builder()
            .type("RegistrationsBaseUrl/Versioned")
            .id("@/api/v3/registration-semver2/")
            .build();

    public static Resource PACKAGE_DISPLAY_METADATA_URI_TEMPLATE_3_0_0_RC = Resource.builder()
            .type("PackageDisplayMetadataUriTemplate/3.0.0-rc")
            .id("@/api/v3/registration/{id-lower}/index.json")
            .build();

    public static Resource PACKAGE_VERSION_DISPLAY_METADATA_URI_TEMPLATE_3_0_0_RC = Resource.builder()
            .type("PackageVersionDisplayMetadataUriTemplate/3.0.0-rc")
            .id("@/api/v3/registration/{id-lower}/{version-lower}.json")
            .build();

    public static Resource LEGACY_GALLERY = Resource.builder()
            .type("LegacyGallery")
            .id("@/api/v2")
            .build();

    public static Resource LEGACY_GALLERY_2_0_0 = Resource.builder()
            .type("LegacyGallery/2.0.0")
            .id("@/api/v2")
            .build();

    public static Resource SEARCH_QUERY_SERVICE = Resource.builder()
            .type("SearchQueryService")
            .id("@/api/v3/query")
            .build();

    public static Resource SEARCH_QUERY_SERVICE_3_0_0_RC = Resource.builder()
            .type("SearchQueryService/3.0.0-rc")
            .id("@/api/v3/query")
            .build();

    public static Resource SEARCH_QUERY_SERVICE_3_0_0_BETA = Resource.builder()
            .type("SearchQueryService/3.0.0-beta")
            .id("@/api/v3/query")
            .build();

    private static final List<Resource> ALL_RESOURCES;

    static {
        List<Resource> resources = new ArrayList<>();
        resources.add(PACKAGE_PUBLISH_2_0_0);
        resources.add(SYMBOL_PACKAGE_PUBLISH_4_9_0);
        resources.add(REGISTRATION_BASE_URL);
        resources.add(REGISTRATION_BASE_URL_3_0_0_BETA);
        resources.add(REGISTRATION_BASE_URL_3_0_0_RC);
        resources.add(REGISTRATION_BASE_URL_3_4_0);
        resources.add(REGISTRATION_BASE_URL_3_6_0);
        resources.add(REGISTRATION_BASE_URL_VERSIONED);
        resources.add(PACKAGE_DISPLAY_METADATA_URI_TEMPLATE_3_0_0_RC);
        resources.add(PACKAGE_VERSION_DISPLAY_METADATA_URI_TEMPLATE_3_0_0_RC);
        resources.add(LEGACY_GALLERY);
        resources.add(LEGACY_GALLERY_2_0_0);
        resources.add(SEARCH_QUERY_SERVICE);
        resources.add(SEARCH_QUERY_SERVICE_3_0_0_RC);
        resources.add(SEARCH_QUERY_SERVICE_3_0_0_BETA);
        ALL_RESOURCES = Collections.unmodifiableList(resources);
    }

    /**
     * @return List<Resource> 预定义的资源列表
     */
    public static List<Resource> getAllResources() {
        List<Resource> deepCopy = new ArrayList<>();
        for (Resource resource : ALL_RESOURCES) {
            deepCopy.add(resource.clone());
        }
        return deepCopy;
    }
}
