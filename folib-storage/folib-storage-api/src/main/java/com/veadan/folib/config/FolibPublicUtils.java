package com.veadan.folib.config;

import com.veadan.folib.storage.repository.Repository;

public class FolibPublicUtils {

    public static String getRepositoryWebServerUrl(Repository repository) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append("http://")
                .append(System.getProperty("folib.host") != null ? System.getProperty("folib.host") : "localhost")
                .append(":")
                .append(System.getProperty("folib.port") != null ?
                        Integer.parseInt(System.getProperty("folib.port")) : 38080).append("/")
                .append(repository.getStorage().getId())
                .append("/")
                .append(repository.getId());
        return urlBuilder.toString();
    }

    public static String getWebServerUrl() {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append("http://")
                .append(System.getProperty("folib.host") != null ? System.getProperty("folib.host") : "localhost")
                .append(":")
                .append(System.getProperty("folib.port") != null ?
                        Integer.parseInt(System.getProperty("folib.port")) : 38080).append("/");
        return urlBuilder.toString();
    }

    public static String getFileUrl(Repository repository, String path) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append("http://")
                .append(System.getProperty("folib.host") != null ? System.getProperty("folib.host") : "localhost")
                .append(":")
                .append(System.getProperty("folib.port") != null ?
                        Integer.parseInt(System.getProperty("folib.port")) : 38080).append("/storages/")
                .append(repository.getStorage().getId())
                .append("/")
                .append(repository.getId())
                .append("/").append(path);
        return urlBuilder.toString();
    }
}
