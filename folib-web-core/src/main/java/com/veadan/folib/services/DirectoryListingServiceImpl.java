package com.veadan.folib.services;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryPath;
import org.apache.commons.lang.StringUtils;
import com.veadan.folib.domain.DirectoryListing;
import com.veadan.folib.domain.FileContent;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DirectoryListingServiceImpl implements DirectoryListingService
{

    private static final Logger logger = LoggerFactory.getLogger(DirectoryListingService.class);

    private String baseUrl;

    public DirectoryListingServiceImpl(String baseUrl)
    {
        super();
        this.baseUrl = StringUtils.chomp(baseUrl.toString(), "/");
    }

    @Override
    public DirectoryListing fromStorages(Map<String, ? extends Storage> storages)
        throws IOException
    {
        DirectoryListing directoryListing = new DirectoryListing();

        for (Storage storage : storages.values())
        {
            FileContent fileContent = new FileContent(storage.getId());
            directoryListing.getDirectories().add(fileContent);

            fileContent.setStorageId(storage.getId());
            fileContent.setUrl(calculateDirectoryUrl(fileContent));
        }

        return directoryListing;
    }

    @Override
    public DirectoryListing fromRepositories(Map<String, ? extends Repository> repositories)
        throws IOException
    {
        DirectoryListing directoryListing = new DirectoryListing();

        for (Repository repository : repositories.values())
        {
            FileContent fileContent = new FileContent(repository.getId());
            directoryListing.getDirectories().add(fileContent);

            fileContent.setStorageId(repository.getStorage().getId());
            fileContent.setRepositoryId(repository.getId());

            fileContent.setUrl(calculateDirectoryUrl(fileContent));
        }

        return directoryListing;
    }

    @Override
    public DirectoryListing fromRepositoryPath(RepositoryPath path)
        throws IOException
    {
        return fromPath(path);
    }

    private DirectoryListing fromPath(Path path)
        throws IOException
    {
        path = path.normalize();

        DirectoryListing directoryListing = new DirectoryListing();

        Map<String, List<FileContent>> content = generateDirectoryListing(path);

        directoryListing.setDirectories(content.get("directories"));
        directoryListing.setFiles(content.get("files"));

        return directoryListing;
    }

    private Map<String, List<FileContent>> generateDirectoryListing(Path path)
        throws IOException
    {
        List<FileContent> directories = new ArrayList<>();
        List<FileContent> files = new ArrayList<>();

        List<Path> contentPaths;
        try (Stream<Path> pathStream = Files.list(path))
        {
            contentPaths = pathStream.filter(p -> !p.toString().startsWith("."))
                    .filter(p -> !p.toString().contains("/.")).filter(p -> {
                        try
                        {
                            return !Files.isHidden(p);
                        }
                        catch (IOException e)
                        {
                            logger.debug("Error accessing path {}", p);
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }

        for (Path contentPath : contentPaths)
        {
            FileContent file = new FileContent(contentPath.getFileName().toString());
            file.setPath(contentPath.toString().replace("folib-vault/logs/", ""));
            Map<String, Object> fileAttributes = Files.readAttributes(contentPath, "*");

            file.setStorageId((String) fileAttributes.get(RepositoryFileAttributeType.STORAGE_ID.getName()));
            file.setRepositoryId((String) fileAttributes.get(RepositoryFileAttributeType.REPOSITORY_ID.getName()));

            file.setArtifactPath((String) fileAttributes.get("artifactPath"));

            if (Boolean.TRUE.equals(fileAttributes.get("isDirectory")))
            {
                file.setUrl(calculateDirectoryUrl(file));

                directories.add(file);

                continue;
            }

            file.setUrl((URL) fileAttributes.get(RepositoryFileAttributeType.RESOURCE_URL.getName()));

            file.setLastModified(new Date(((FileTime) fileAttributes.get("lastModifiedTime")).toMillis()));
            file.setSize((Long) fileAttributes.get("size"));

            files.add(file);
        }

        Map<String, List<FileContent>> listing = new HashMap<>();
        listing.put("directories", directories);
        listing.put("files", files);

        return listing;
    }

    /**
     * @param rootPath
     *            The root path in which directory listing is allowed. Used as a
     *            precaution to prevent directory traversing.
     *            When "path" is outside "rootPath" an exception will be thrown.
     * @param path
     *            The path which needs to be listed
     * @return DirectoryListing
     * @throws RuntimeException
     *             when path is not within rootPath.
     */
    public DirectoryListing fromPath(Path rootPath,
                                     Path path)
        throws IOException
    {
        rootPath = rootPath.normalize();
        path = path.normalize();

        if (!path.equals(rootPath) && !path.startsWith(rootPath))
        {
            String message = String.format(
                                           "Requested directory listing for [%s] is outside the scope of the root path [%s]! Possible intrusion attack or misconfiguration!",
                                           path, rootPath);
            logger.error(message);
            throw new RuntimeException(message);
        }

        return fromPath(path);
    }

    private URL calculateDirectoryUrl(FileContent file)
        throws MalformedURLException
    {
        if (file.getRepositoryId() == null)
        {

            return new URL(String.format("%s/%s", baseUrl, file.getStorageId()));

        }
        else if (file.getArtifactPath() == null)
        {

            return new URL(String.format("%s/%s/%s", baseUrl, file.getStorageId(),
                                         file.getRepositoryId()));

        }

        return new URL(String.format("%s/%s/%s/%s", baseUrl, file.getStorageId(),
                                     file.getRepositoryId(), file.getArtifactPath()));
    }

}
