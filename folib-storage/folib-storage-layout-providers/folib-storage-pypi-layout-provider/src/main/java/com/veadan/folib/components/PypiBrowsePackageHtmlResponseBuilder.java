package com.veadan.folib.components;

/**
 * @author leipenghui
 * @date 2024/7/2
 **/

import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.pypi.PypiSearchResult;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

@Component
public class PypiBrowsePackageHtmlResponseBuilder {

    public String getHtmlResponse(List<Path> filePaths)
            throws IOException {

        String htmlResponse = "";
        if (CollectionUtils.isEmpty(filePaths)) {
            htmlResponse = "<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Not Found</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Not Found</h1>\n" +
                    "        </body>\n" +
                    "</html>";
        } else {

            PypiArtifactCoordinates artifactCoordinates = (PypiArtifactCoordinates) RepositoryFiles.readCoordinates((RepositoryPath) filePaths.get(0));
            final String packageName = artifactCoordinates.getId();

            htmlResponse = "<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Links for " + packageName + "</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Links for " + packageName + "</h1>\n" +
                    "                   " + getPackageLinks(filePaths) +
                    "        </body>\n" +
                    "</html>";
        }
        return htmlResponse;
    }

    private String getPackageLinks(List<Path> filePaths)
            throws IOException {

        StringBuilder packageLinks = new StringBuilder();

        for (Path path : filePaths) {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            PypiArtifactCoordinates artifactCoordinates = (PypiArtifactCoordinates) RepositoryFiles.readCoordinates(repositoryPath);

            Repository repository = repositoryPath.getRepository();
            packageLinks.append("<a href=\"" + "/artifactory/api/pypi/").append(repository.getId()).append("/packages/").append(artifactCoordinates.buildPath()).append("\">").append(artifactCoordinates.getFileName()).append("</a><br>\n");
        }

        return packageLinks.toString();
    }

    public String getProxyHtmlResponse(List<PypiSearchResult> pypiSearchResultList) {
        String htmlResponse = "";
        if (CollectionUtils.isEmpty(pypiSearchResultList)) {
            htmlResponse = "<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Not Found</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Not Found</h1>\n" +
                    "        </body>\n" +
                    "</html>";
        } else {
            PypiArtifactCoordinates artifactCoordinates = PypiArtifactCoordinates.parse(pypiSearchResultList.get(0).getArtifactName());
            final String packageName = artifactCoordinates.getId();
            htmlResponse = "<html>\n" +
                    "        <head>\n" +
                    "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                    "            <title>Links for " + packageName + "</title>\n" +
                    "        </head>\n" +
                    "        <body>\n" +
                    "            <h1>Links for " + packageName + "</h1>\n" +
                    "                   " + getProxyPackageLinks(pypiSearchResultList) +
                    "        </body>\n" +
                    "</html>";
        }
        return htmlResponse;
    }

    private String getProxyPackageLinks(List<PypiSearchResult> pypiSearchResultList) {
        StringBuilder packageLinks = new StringBuilder();
        PypiArtifactCoordinates artifactCoordinates = null;
        for (PypiSearchResult pypiSearchResult : pypiSearchResultList) {
            artifactCoordinates = PypiArtifactCoordinates.parse(pypiSearchResult.getArtifactName());
            packageLinks.append("<a href=\"" + "/artifactory/api/pypi/").append(pypiSearchResult.getRepositoryId()).append("/packages/").append(pypiSearchResult.getArtifactPath()).append("\"");
            if (StringUtils.isNotBlank(pypiSearchResult.getAttributes())) {
                packageLinks.append(" ");
                packageLinks.append(pypiSearchResult.getAttributes());
            }
            packageLinks.append(">");
            packageLinks.append(artifactCoordinates.getFileName()).append("</a><br>\n");
        }
        return packageLinks.toString();
    }

    public String nouFound() {
        return "<html>\n" +
                "        <head>\n" +
                "            <meta name=\"pypi:repository-version\" content=\"1.0\">\n" +
                "            <title>Not Found</title>\n" +
                "        </head>\n" +
                "        <body>\n" +
                "            <h1>Not Found</h1>\n" +
                "        </body>\n" +
                "</html>";
    }

}
