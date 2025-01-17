package com.veadan.folib.web;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * @author veadan
 */
public final class Constants {

    private Constants() {
    }

    public static final String ARTIFACT_ROOT_PATH = "/storages";

    public final static String ARTIFACTORY_ARTIFACT_ROOT_PATH = "/artifactory";

    public static final String PYPI_ARTIFACT_ROOT_PATH = "/artifactory/api/pypi/";

    public static final String NPM_ARTIFACT_ROOT_PATH = "/artifactory/api/npm/";

    public static final String OHPM_ARTIFACT_ROOT_PATH = "/artifactory/api/ohpm/";

    public static final String PHP_ARTIFACT_ROOT_PATH = "/artifactory/api/composer/";

    public static final String CONAN_ARTIFACT_ROOT_PATH = "/artifactory/api/conan/";

    public static final String HELM_ARTIFACT_ROOT_PATH = "/artifactory/api/helm/";

    public static final String PODS_ARTIFACT_ROOT_PATH = "/artifactory/api/pods/";

    public static final String GO_ARTIFACT_ROOT_PATH = "/artifactory/api/go/";

    public static final String LFS_ARTIFACT_ROOT_PATH = "/artifactory/api/lfs/";

    public static final String HUGGING_FACE_ML_ARTIFACT_ROOT_PATH = "/artifactory/api/huggingfaceml/";

    public static final String PUB_ARTIFACT_ROOT_PATH = "/artifactory/api/pub/";

    public static final String CARGO_ARTIFACT_ROOT_PATH = "/artifactory/api/cargo/";

    public static final List<String> ARTIFACT_API_ROOT_PATHS = Lists.newArrayList(PYPI_ARTIFACT_ROOT_PATH, NPM_ARTIFACT_ROOT_PATH, OHPM_ARTIFACT_ROOT_PATH, PHP_ARTIFACT_ROOT_PATH, CONAN_ARTIFACT_ROOT_PATH, HELM_ARTIFACT_ROOT_PATH, PODS_ARTIFACT_ROOT_PATH, GO_ARTIFACT_ROOT_PATH, LFS_ARTIFACT_ROOT_PATH, HUGGING_FACE_ML_ARTIFACT_ROOT_PATH, PUB_ARTIFACT_ROOT_PATH, CARGO_ARTIFACT_ROOT_PATH);

    public static final List<String> ARTIFACT_ROOT_PATHS = Lists.newArrayList(ARTIFACT_ROOT_PATH, ARTIFACTORY_ARTIFACT_ROOT_PATH);

    public static final String REPOSITORY_REQUEST_ATTRIBUTE = Constants.class.getName() + ".repository";

    public static final String STORAGE_NOT_FOUND_REQUEST_ATTRIBUTE = Constants.class.getName() + ".storageNotFound";

    public static final String REPOSITORY_NOT_FOUND_REQUEST_ATTRIBUTE =
            Constants.class.getName() + ".repositoryNotFound";

    public static final String DOCKER_ROOT_PATH = "/v2";

    public static final String STORAGE_ROOT_PATH = "/api/configuration/folib/storages";

    public static final String ARTIFACT_COPY_PATH = ARTIFACT_ROOT_PATH + "/copy";

}
