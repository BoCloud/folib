package com.veadan.folib.providers.repository;

import com.google.common.collect.Maps;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.config.FolibPublicUtils;
import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.*;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Component
public class HostedRepositoryProvider extends AbstractRepositoryProvider
{

    private static final Logger logger = LoggerFactory.getLogger(HostedRepositoryProvider.class);

    private static final String ALIAS = "hosted";

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    
    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    
    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    protected InputStream getInputStreamInternal(RepositoryPath repositoryPath) throws IOException
    {
        try
        {
            return Files.newInputStream(repositoryPath);
        }
        catch (ArtifactNotFoundException e) 
        {
            logger.info("The path [{}] does not exist!\n*\t[{}]", repositoryPath, e.getMessage());

            return null;
        }
        catch (IOException ex)
        {
            logger.error("Failed to decorate InputStream for [{}]", repositoryPath, ex);
            
            throw ex;
        }
    }

    @Override
    public OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
            throws IOException
    {
        return Files.newOutputStream(repositoryPath);
    }

    @Override
    public List<Path> search(String storageId,
                             String repositoryId,
                             RepositorySearchRequest predicate,
                             Paginator paginator)
    {
        List<Path> result = new LinkedList<Path>();

        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository);
        long startTime = System.currentTimeMillis();
        List<Artifact> searchResult = artifactIdGroupRepository.findArtifactsGremlin(storageId, repositoryId, predicate.getArtifactId(),
                                                                              predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit());
        logger.info("FindArtifacts storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, predicate.getArtifactId(), predicate.getCoordinateValues(), paginator.getSkip(), paginator.getLimit(), paginator.getUseLimit(), searchResult.size(), System.currentTimeMillis() - startTime);
        for (Artifact artifactEntry : searchResult)
        {
            
            try
            {
                result.add(rootRepositoryPath.resolve(artifactEntry));
            }
            catch (Exception e)
            {
                logger.error("Failed to resolve Artifact [{}]",
                             artifactEntry.getArtifactCoordinates(), e);
                continue;
            }
        }
        return result;
    }

    @Override
    public Long count(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate)
    {
        return artifactIdGroupRepository.commonCountArtifacts(storageId, repositoryId, predicate.getArtifactId(),
                                                        predicate.getCoordinateValues());
    }

    @Override
    public Map<String, Object> searchConanPackage(Repository repository, String query) throws IOException {
        // 本地仓查询
        query = StringUtils.isBlank(query) ? "" : query.trim();
        final String name;
        final String version;
        if ("".equals(query)) {
            name = "";
            version = null;
        } else {
            //todo 后期提取到常量
            Pattern compile = Pattern.compile("^(?<name>[A-Za-z0-9_\\-.]+?)(/(?<version>[0-9.]*?)){0,1}$");
            Matcher matcher = compile.matcher(query);
            if (!matcher.matches()) {
                return new HashMap<>(0);
            }
            name = matcher.group("name");
            version = matcher.group("version");
        }
        // 查询 repository path 目录
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(
                repository.getStorage().getId(),
                repository.getId(),
                name);
        if (!Files.exists(repositoryPath)) {
            return Map.of("results", new ArrayList<>());
        }
        if (StringUtils.isBlank(name)) {
            List<String> list = Files.list(repositoryPath)
                    .flatMap(p -> {
                        try {
                            return Files.list(p);
                        } catch (IOException e) {
                            return null;
                        }
                    })
                    .map(p -> (RepositoryPath) p)
                    .map(RepositoryPath::relativize)
                    .map(Path::toString)
                    .map(p -> p.replace("\\", "/"))
                    .collect(Collectors.toList());
            return Map.of("results", list);
        } else {
            List<String> list = Files.list(repositoryPath)
                    .filter(p -> version == null || p.endsWith(version))
                    .map(a -> (RepositoryPath) a)
                    .map(RepositoryPath::relativize)
                    .map(Path::toString)
                    .map(p -> p.replace("\\", "/"))
                    .collect(Collectors.toList());
            return Map.of("results", list);
        }
    }

    @Override
    public Map<String, Object> searchConanDownLoadUrl(Repository repository, String name, String version, String username, String channel) {
        String url = FolibPublicUtils.getRepositoryWebServerUrl(repository);
//        Map<String, Object> map = new HashMap<String, Object>();
//        String format = String.format("%s/v1/files/%s/%s/%s/%s/export/%s", url, name, version, username, channel);
//        String conanExportTgz = url + "/v1/files/_/" + name + "/" + version + "/_/0/export/conan_export.tgz";
//        String conanManifestTxt = url + "/v1/files/_/" + name + "/" + version + "/_/0/export/conanmanifest.txt";
//        String conanFilePy = url + "/v1/files/_/" + name + "/" + version + "/_/0/export/conanfile.py";
//        map.put("conan_export.tgz", conanExportTgz);
//        map.put("conanmanifest.txt", conanManifestTxt);
//        map.put("conanfile.py", conanFilePy);
        List<String> list = List.of("conan_export.tgz", "conanmanifest.txt", "conanfile.py");
        return list.stream().collect(Collectors.toMap(
                filename -> filename,
                filename -> String.format("%s/v1/files/%s/%s/%s/%s/export/%s", url, name, version, username, channel, filename)));
    }


    @Override
    public Map<String, Object> searchConanPackageInfo(Repository repository, String packageName, String version) throws IOException {
        // 查询本地仓的package id 及详情  _/zulu-openjdk/11.0.15/_/0/package
        String packagePath = "_/" + packageName + "/" + version + "/_/0/package";
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId()
                , repository.getId(), packagePath);
        if (!Files.exists(repositoryPath)) {
            logger.error("Package path {} not found", repositoryPath);
            return Maps.newHashMap();
        }
        Map<String, Object> resultMap = new HashMap<>();
        List list = Files.list(repositoryPath).collect(Collectors.toList());
        list.forEach(x -> {
            try {
                RepositoryPath packageIdPath = (RepositoryPath) x;
                String a = packagePath + "/" + "";
                Map<String, Object> resultInfoMap = new HashMap<>();
                String[] packageArray = packageIdPath.relativize().toString().split("/");
                String packageId = packageArray.length > 0 ? packageArray[packageArray.length - 1] : "";
                String packageIdPathStr = packagePath + "/" + packageId + "/0/conaninfo.txt";
                RepositoryPath packageIdInfoPath = repositoryPathResolver.resolve(repository.getStorage().getId()
                        , repository.getId(), packageIdPathStr);
                List<String> infoList = Files.readAllLines(packageIdInfoPath);
                resultMap.put(packageId, resultInfoMap);
                Map<String, Map<String, Object>> settingsMap = new HashMap<>();
                Map<String, Map<String, Object>> optionsMap = new HashMap<>();
                List<String> requireList = new ArrayList<>();
                resultInfoMap.put("settings", settingsMap);
                resultInfoMap.put("options", optionsMap);
                resultInfoMap.put("requires", requireList);
                resultInfoMap.put("recipe_hash", "");
                String key = "";
                for (String line : infoList) {
                    line = line.trim();
                    if (line.equals("[settings]")) {
                        key = "settings";
                        continue;
                    } else if (line.equals("[options]")) {
                        key = "options";
                        continue;
                    } else if (line.equals("[full_requires]")) {
                        key = "requires";
                        continue;
                    } else if (line.equals("[recipe_hash]")) {
                        key = "recipe_hash";
                        continue;
                    } else if (line.contains("[") && line.contains("]")) {
                        key = "other";
                        continue;
                    }
                    if (org.junit.platform.commons.util.StringUtils.isBlank(line)) {
                        continue;
                    }

                    if (key.equals("settings") || key.equals("options")) {
                        Map<String, Object> map = (Map<String, Object>) resultInfoMap.get(key);
                        if (line.contains("=")) {
                            map.put(line.split("=")[0], line.split("=")[1]);
                        }
                    } else if (key.equals("requires")) {
                        List<String> listRequires = (List<String>) resultInfoMap.get(key);
                        listRequires.add(line);
                    } else if (key.equals("recipe_hash")) {
                        resultInfoMap.put("recipe_hash", line);
                    }
                }
            } catch (Exception e) {
                logger.error("get package info error {}", e.getMessage());
            }

        });
        return resultMap;
    }

    @Override
    protected RepositoryPath fetchPath(RepositoryPath repositoryPath) 
           throws IOException
    {
        logger.info(" -> Checking local cache for {} ...", repositoryPath);
        if (artifactNotExists(repositoryPath))
        {
            logger.info("The artifact {} was not found in the local cache", repositoryPath);

            return null;
        }
        
        logger.info("The artifact {} was found in the local cache", repositoryPath);
        return repositoryPath;
    }

    private boolean artifactNotExists(RepositoryPath repositoryPath) throws IOException
    {
        return !RepositoryFiles.artifactExists(repositoryPath);
    }

}
