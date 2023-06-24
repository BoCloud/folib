package com.veadan.folib.controllers.layout.pypi;

import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.pypi.PypiSearchResult;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;

/**
 * Helper class for {@link PypiArtifactController}
 *
 * @author ankit.tomar
 */
@Component
public class PypiBrowsePackageHtmlResponseBuilder {

    public String getHtmlResponse(List<Path> filePaths)
            throws IOException {

        String htmlResponse = "";
        if (CollectionUtils.isEmpty(filePaths)) {
            htmlResponse = "<html>\n" +
                    "        <head>\n" +
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

        String packageLinks = "";

        for (Path path : filePaths) {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            PypiArtifactCoordinates artifactCoordinates = (PypiArtifactCoordinates) RepositoryFiles.readCoordinates(repositoryPath);

            Repository repository = repositoryPath.getRepository();
            packageLinks += "<a href=\"" + "/storages/" + repository.getStorage().getId() + "/" + repository.getId() +
                    "/packages/" + artifactCoordinates.getFileName() + "\">" +
                    artifactCoordinates.getFileName() + "</a><br>\n";
        }

        return packageLinks;
    }

    public String getProxyHtmlResponse(List<PypiSearchResult> pypiSearchResultList) {
        String htmlResponse = "";
        if (CollectionUtils.isEmpty(pypiSearchResultList)) {
            htmlResponse = "<html>\n" +
                    "        <head>\n" +
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
        String packageLinks = "";
        PypiArtifactCoordinates artifactCoordinates = null;
        for (PypiSearchResult pypiSearchResult : pypiSearchResultList) {
            artifactCoordinates = PypiArtifactCoordinates.parse(pypiSearchResult.getArtifactName());
            packageLinks += "<a href=\"" + "/storages/" + pypiSearchResult.getStorageId() + "/" + pypiSearchResult.getRepositoryId() +
                    "/packages/" + artifactCoordinates.getFileName() + "\">" +
                    artifactCoordinates.getFileName() + "</a><br>\n";
        }
        return packageLinks;
    }

    public String nouFound() {
        return "<html>\n" +
                "        <head>\n" +
                "            <title>Not Found</title>\n" +
                "        </head>\n" +
                "        <body>\n" +
                "            <h1>Not Found</h1>\n" +
                "        </body>\n" +
                "</html>";
    }

}
