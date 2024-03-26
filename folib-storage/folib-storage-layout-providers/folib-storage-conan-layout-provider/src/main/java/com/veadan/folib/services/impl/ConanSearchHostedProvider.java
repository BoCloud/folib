package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.veadan.folib.artifact.coordinates.ConanArtifactIndex;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ConanRevisions;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.io.RootRepositoryPath;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ConanSearchProvider;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class ConanSearchHostedProvider implements ConanSearchProvider {

    @Inject
    private ConanSearchProviderRegistry conanSearchProviderRegistry;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        conanSearchProviderRegistry.addProvider(ConanSearchRepositoryTypeEnum.CONAN_HOSTED.getType(), this);
        log.info("Registered conan search provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), ConanSearchRepositoryTypeEnum.CONAN_HOSTED.getType());
    }

    @Override
    public SearchResults search(Repository repository, String query) {
        if (StringUtils.isNotBlank(query) && query.contains(GlobalConstants.ASTERISK)) {
            query = query.replaceAll("\\*", ".*");
        }
        SearchResults searchResults = SearchResults.builder().results(Lists.newArrayList()).build();
        RootRepositoryPath rootRepositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId());
        try (Stream<Path> pathStream = Files.walk(rootRepositoryPath)) {
            String finalQuery = query;
            pathStream.filter(item -> !Files.isDirectory(item) && ConanArtifactIndex.isReferenceIndexJSON(item))
                    .sorted()
                    .forEach(item -> {
                        try {
                            ConanRevisions conanRevisions = JSONObject.parseObject(Files.readString(item), ConanRevisions.class);
                            if (Objects.nonNull(conanRevisions) && StringUtils.isNotBlank(conanRevisions.getReference())) {
                                boolean flag = StringUtils.isBlank(finalQuery) || (StringUtils.isNotBlank(finalQuery) && (conanRevisions.getReference().startsWith(finalQuery) || Pattern.matches(finalQuery, conanRevisions.getReference())));
                                if (flag) {
                                    searchResults.getResults().add(conanRevisions.getReference());
                                }
                            }
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return searchResults;
    }

    @Override
    public JSONObject revisionsSearch(Repository repository, String artifactPath, String url) {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), artifactPath);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            return null;
        }
        String conaninfo = "conaninfo.txt";
        JSONObject data = new JSONObject();
        try (Stream<Path> pathStream = Files.walk(repositoryPath)) {
            pathStream.filter(item -> !Files.isDirectory(item) && item.getFileName().toString().equals(conaninfo))
                    .sorted()
                    .forEach(item -> {
                        try {
                            JSONObject subData = new JSONObject();
                            subData.put("requires", new JSONArray());
                            String content = Files.readString(item);
                            subData.put("content", content);
                            resolveContent(subData, content);
                            data.put(item.getParent().getFileName().toString(), subData);
                        } catch (Exception ex) {
                            log.error(ExceptionUtils.getStackTrace(ex));
                        }
                    });
        } catch (IOException ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return data;
    }

    private void resolveContent(JSONObject data, String content) {
        // 使用正则表达式匹配带括号的键
        final String regex = "\\[(.*?)\\]";
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(content);
        // 循环匹配键
        while (matcher.find()) {
            String key = matcher.group(1);
            data.put(key, getPackageInfo(content, key));
        }
    }

    private static Map<String, String> getPackageInfo(String content, String key) {
        if (StringUtils.isBlank(content)) {
            return null;
        }
        boolean flag = false;
        Map<String, String> map = Maps.newLinkedHashMap();
        String[] lines = content.split("\\r?\\n");
        for (String line : lines) {
            if (key.equalsIgnoreCase(line.trim())) {
                flag = true;
                continue;
            } else if (line.trim().startsWith("[")) {
                flag = false;
            }
            if (flag && StringUtils.isNotBlank(line.trim())) {
                String[] keyValue = line.split("=", 2);
                if (keyValue.length == 2) {
                    String itemKey = keyValue[0].trim();
                    String itemValue = keyValue[1].trim();
                    map.put(itemKey, itemValue);
                }
            }
        }
        return map;
    }

}
