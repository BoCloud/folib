/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.utils;

import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.folib.extractor.CargoMetadataExtractor;
import com.folib.model.CargoDependencyMetadata;
import com.folib.model.CargoMetadata;
import com.folib.providers.io.RepositoryPath;

import com.folib.services.ArtifactManagementService;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.SetMultimap;
import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayInputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.function.Supplier;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.tomlj.Toml;
import org.tomlj.TomlArray;
import org.tomlj.TomlParseResult;
import org.tomlj.TomlTable;

public final class CargoUtil {
    private static final Logger log = LoggerFactory.getLogger(CargoUtil.class);

    static final String COLLECTION_DELIMITER = ";";

    static final String CARET = "^";

    private CargoUtil() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    private static final ObjectMapper MAPPER = initObjectMapper();

    private static final Set<String> DEPENDENCIES_TYPES = Set.of("dependencies", "dev-dependencies", "build-dependencies");

    public static final Integer MAX_SEARCH_PER_PAGE = 100;

    public static final Integer DEFAULT_SEARCH_PER_PAGE = 10;

    /**
     * 解析Cargo配置信息
     * 该方法用于解析以字符串形式提供的Cargo配置内容，并返回一个CargoMetadata对象，其中包含解析出的配置数据
     *
     * @param configContent Cargo配置的字符串表示，通常是从配置文件中读取或以其他方式获取的配置信息
     * @return CargoMetadata对象，包含解析后的配置数据，如版本号、依赖库等信息
     */
    public static CargoMetadata parseCargoConfig(String configContent) {
        // 检查配置内容是否为null，如果为null则抛出NullPointerException异常
        if (configContent == null)
            throw new NullPointerException("configContent is marked non-null but is null");

        // 创建CargoMetadata对象以存储解析后的配置信息
        CargoMetadata ret = new CargoMetadata();

        // 解析Toml格式的配置内容
        TomlParseResult result = Toml.parse(configContent);

        // 获取配置中的"package"部分，如果不存在则返回null
        Object pack = result.get("package");
        if (pack == null)
            return null;

        // 将"package"部分转换为TomlTable类型以进一步操作
        TomlTable cargoPackage = (TomlTable) pack;

        // 从"package"部分中获取并设置CargoMetadata对象的属性
        ret.setName(cargoPackage.getString("name"));
        ret.setVers(cargoPackage.getString("version"));
        ret.setDescription(cargoPackage.getString("description"));
        ret.setDocumentation(cargoPackage.getString("documentation"));
        ret.setHomepage(cargoPackage.getString("homepage"));
        ret.setRepository(cargoPackage.getString("repository"));
        ret.setLicense(cargoPackage.getString("license"));
        ret.setLicenseFile(cargoPackage.getString("license-file"));

        // 获取并设置"package"部分中的"authors"，如果存在则调用convertTomlArrayToList方法转换
        TomlArray authors = cargoPackage.getArrayOrEmpty("authors");
        if (!authors.isEmpty())
            ret.setAuthors(convertTomlArrayToList(authors));

        // 获取并设置"package"部分中的"keywords"，如果存在则调用convertTomlArrayToList方法转换
        TomlArray keywords = cargoPackage.getArrayOrEmpty("keywords");
        if (!keywords.isEmpty())
            ret.setKeywords(convertTomlArrayToList(keywords));

        // 获取并设置"package"部分中的"categories"，如果存在则调用convertTomlArrayToList方法转换
        TomlArray categories = cargoPackage.getArrayOrEmpty("categories");
        if (!categories.isEmpty())
            ret.setCategories(convertTomlArrayToList(categories));

        // 获取并设置配置中的"dependencies"部分，如果存在则调用getDependencies方法处理
        TomlTable dependencies = (TomlTable) result.get("dependencies");
        if (dependencies != null)
            ret.setDeps(getDependencies(dependencies, CargoDependencyKind.NORMAL));

        // 获取并设置配置中的"dev-dependencies"部分，如果存在则调用getDependencies方法处理并添加到依赖列表中
        TomlTable devDependencies = (TomlTable) result.get("dev-dependencies");
        if (devDependencies != null)
            ret.getDeps().addAll(getDependencies(devDependencies, CargoDependencyKind.DEV));

        // 获取并设置配置中的"build-dependencies"部分，如果存在则调用getDependencies方法处理并添加到依赖列表中
        TomlTable buildDependencies = (TomlTable) result.get("build-dependencies");
        if (buildDependencies != null)
            ret.getDeps().addAll(getDependencies(buildDependencies, CargoDependencyKind.BUILD));

        // 获取并设置配置中的"features"部分，如果存在则调用getLineFeatures方法处理并设置到CargoMetadata对象中
        TomlTable features = (TomlTable) result.get("features");
        if (features != null) {
            Map<String, List<String>> c = (Map<String, List<String>>) features.toMap().entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, v -> getLineFeatures(v.getValue())));
            ret.setFeatures(c);
        }

        // 获取并设置配置中的"target"部分，如果存在则调用getDependenciesFromTarget方法处理并添加到依赖列表中
        TomlTable target = (TomlTable) result.get("target");
        if (target != null)
            ret.getDeps().addAll(getDependenciesFromTarget(target));
        return ret;
    }

    /**
     * 从目标对象中获取依赖列表
     *
     * @param target 目标对象，通常是一个TomlTable对象，表示目标配置信息
     * @return 依赖列表，由CargoDependencyMetadata对象表示
     */
    private static List<CargoDependencyMetadata> getDependenciesFromTarget(TomlTable target) {
        List<CargoDependencyMetadata> results = Lists.newArrayList();
        Map<String, Object> targets = target.toMap();
        targets.forEach((key, value) -> {
            if (value instanceof TomlTable) {
                TomlTable attributes = (TomlTable) value;
                for (String propertyName : DEPENDENCIES_TYPES) {
                    TomlTable tomlTableDependencies = attributes.getTable(propertyName);
                    if (tomlTableDependencies != null) {
                        List<CargoDependencyMetadata> dependencies = getDependencies(tomlTableDependencies, CargoDependencyKind.fromType(propertyName), key);
                        results.addAll(dependencies);
                    }
                }
            }
        });
        return results;
    }

    /**
     * 从属性对象中获取行特征
     *
     * @param o 属性对象，通常是一个TomlArray对象，表示属性信息
     * @return 行特征列表，由字符串表示
     */
    private static List<String> getLineFeatures(Object o) {
        if (!(o instanceof TomlArray))
            return List.of();
        Objects.requireNonNull(String.class);
        Objects.requireNonNull(String.class);
        return ((TomlArray) o).toList().stream().filter(String.class::isInstance).map(String.class::cast).collect(Collectors.toList());
    }

    /**
     * 从依赖对象中获取依赖列表
     * @param dependencies 依赖对象，通常是一个TomlTable对象，表示依赖信息
     * @param dependencyKind 依赖类型，可以是NORMAL、DEV、BUILD等
     * @return 依赖列表，由CargoDependencyMetadata对象表示
     */
    private static List<CargoDependencyMetadata> getDependencies(TomlTable dependencies, CargoDependencyKind dependencyKind) {
        return getDependencies(dependencies, dependencyKind, null);
    }

    /**
     * 从依赖对象中获取依赖列表
     *
     * @param dependencies 依赖对象，通常是一个TomlTable对象，表示依赖信息
     * @param dependencyKind 依赖类型，可以是NORMAL、DEV、BUILD等
     * @param dependencyTarget 目标依赖，通常是一个字符串，表示依赖的目标
     * @return 依赖列表，由CargoDependencyMetadata对象表示
     */
    private static List<CargoDependencyMetadata> getDependencies(TomlTable dependencies, CargoDependencyKind dependencyKind, String dependencyTarget) {
        List<CargoDependencyMetadata> results = Lists.newArrayList();
        Map<String, Object> deps = dependencies.toMap();
        deps.forEach((key, value) -> {
            if (value instanceof TomlTable) {
                TomlTable attributes = (TomlTable) value;
                results.add(getCargoDependencyMetadata(dependencyKind, dependencyTarget, key, attributes));
            }
        });
        return results;
    }

    /**
     * 从依赖对象中获取依赖信息
     *
     * @param dependencyKind 依赖类型，可以是NORMAL、DEV、BUILD等
     * @param dependencyTarget 目标依赖，通常是一个字符串，表示依赖的目标
     * @param key 依赖名称，通常是一个字符串
     * @param attributes 依赖属性对象，通常是一个TomlTable对象，表示依赖的属性信息
     * @return 依赖信息，由CargoDependencyMetadata对象表示
     */
    private static CargoDependencyMetadata getCargoDependencyMetadata(CargoDependencyKind dependencyKind, String dependencyTarget, String key, TomlTable attributes) {
        Objects.requireNonNull(dependencyKind);
        return CargoDependencyMetadata.builder().name(key).versionReq(getVersionReqFromAttributes(attributes)).features(getFeaturesFromAttributes(attributes)).optional(attributes.getBoolean("optional", () -> false)).defaultFeatures(getDefaultFeaturesFromAttributes(attributes)).target(attributes.getString("target", () -> dependencyTarget)).kind(attributes.getString("kind", dependencyKind::getKind))
                .packageExplicitName(attributes.getString("package"))
                .registry(getRegistryFromAttributes(attributes))
                .build();
    }


    /**
     * 从依赖属性对象中获取版本要求
     *
     * @param attributes 依赖属性对象，通常是一个TomlTable对象，表示依赖的属性信息
     * @return 版本要求，通常是一个字符串
     */
    private static String getVersionReqFromAttributes(TomlTable attributes) {
        String version = Optional.<String>ofNullable(attributes.getString("versionReq")).orElse(Optional.<String>ofNullable(attributes.getString("version")).orElse(""));
        if (!StringUtils.isBlank(version) && Character.isLetterOrDigit(version.charAt(0)))
            version = "^" + version;
        return version;
    }


    /**
     * 从依赖属性对象中获取特征列表
     *
     * @param attributes 依赖属性对象，通常是一个TomlTable对象，表示依赖的属性信息
     * @return 特征列表，由字符串表示
     */
    private static List<String> getFeaturesFromAttributes(TomlTable attributes) {
        List<String> features;
        Object featuresObj = attributes.get("features");
        if (featuresObj instanceof String) {
            String tempString = (String) featuresObj;
            features = List.of(Arrays.toString((Object[]) tempString.split(";")));
        } else if (featuresObj instanceof TomlArray) {
            TomlArray tomlArray = (TomlArray) featuresObj;
            Objects.requireNonNull(String.class);
            Objects.requireNonNull(String.class);
            features = tomlArray.toList().stream().filter(String.class::isInstance).map(String.class::cast).collect(Collectors.toList());
        } else {
            features = List.of();
        }
        return features;
    }

    /**
     * 从依赖属性对象中获取默认特征
     *
     * @param attributes 依赖属性对象，通常是一个TomlTable对象，表示依赖的属性信息
     * @return 默认特征，布尔值表示
     */
    private static boolean getDefaultFeaturesFromAttributes(TomlTable attributes) {
        return attributes.getBoolean("defaultFeatures", () -> true);
    }

    /**
     * 从依赖属性对象中获取注册中心
     *
     * @param attributes 依赖属性对象，通常是一个TomlTable对象，表示依赖的属性信息
     * @return 注册中心，通常是一个字符串
     */
    private static String getRegistryFromAttributes(TomlTable attributes) {
        return Optional.<String>ofNullable(attributes.getString("registry"))
                .orElse(attributes.getString("registry-index"));
    }

    /**
     * 将字符串转换为CargoMetadata对象
     *
     * @param content 字符串内容，通常是一个Cargo.toml文件的内容
     * @return CargoMetadata对象，表示Cargo.toml文件中的元数据信息
     * @throws JsonProcessingException 如果解析字符串内容失败，抛出该异常
     */
    public static CargoMetadata stringToMetadata(String content) throws JsonProcessingException {
        return (CargoMetadata) MAPPER.readValue(content, CargoMetadata.class);
    }

    /**
     * 将特征列表转换为字符串
     *
     * @param features 特征列表，由字符串表示
     * @return 字符串，表示特征列表
     * @throws JsonProcessingException 如果转换失败，抛出该异常
     */
    public static String featuresToString(Map<String, List<String>> features) throws JsonProcessingException {
        return MAPPER.writeValueAsString(features);
    }

    /**
     * 将特征列表转换为字符串
     *
     * @param features 特征列表，由字符串表示
     * @return 字符串，表示特征列表
     */
    public static Map<String, List<String>> stringToFeatures(String features) {
        Map<String, List<String>> ret = Map.of();
        if (StringUtils.isNotBlank(features)) {
            TypeReference<HashMap<String, List<String>>> typeRef = new TypeReference<HashMap<String, List<String>>>() {

            };
            try {
                ret = (Map<String, List<String>>) MAPPER.readValue(features, typeRef);
            } catch (JsonProcessingException e) {
                log.warn("Failed to parse features field {}", features);
            }
        }
        return ret;
    }

    /**
     * 生成索引行内容
     *
     * @param metadata CargoMetadata对象，表示Cargo.toml文件中的元数据信息
     * @param sha256   SHA256哈希值，通常用于校验文件 integrity
     * @param yanked   是否已删除，布尔值表示
     * @return 索引行内容，通常是一个字符串
     * @throws JsonProcessingException 如果转换失败，抛出该异常
     */
    public static String generateIndexLine(CargoMetadata metadata, String sha256, boolean yanked) throws JsonProcessingException {
        if (metadata.getDeps() == null)
            metadata.setDeps(List.of());
        if (metadata.getFeatures() == null)
            metadata.setFeatures(Map.of());
        List<CargoIndexDependency> dependencies = metadata.getDeps().stream().map(CargoIndexDependency::toIndexDependency).collect(Collectors.toList());
        CargoIndexLine indexLine = new CargoIndexLine(metadata.getName(), metadata.getVers(), dependencies, sha256, metadata.getFeatures(), yanked, metadata.getLinks());
        return MAPPER.writeValueAsString(indexLine);
    }

    /**
     * 计算索引路径
     *
     * @param crateName 仓库名称，通常是一个字符串
     * @return 索引路径，通常是一个字符串
     */
    public static String calculateIndexPath(String crateName) {
        String ret;
        if (crateName.length() == 1) {
            ret = "1/" + crateName;
        } else if (crateName.length() == 2) {
            ret = "2/" + crateName;
        } else if (crateName.length() == 3) {
            ret = "3/" + crateName.charAt(0) + "/" + crateName;
        } else {
            ret = crateName.substring(0, 2) + "/" + crateName.substring(2, 4) + "/" + crateName;
        }
        return ret;
    }

    /**
     * 将依赖列表转换为字符串列表
     *
     * @param dependencies 依赖列表，由CargoDependencyMetadata对象表示
     * @return 字符串列表，表示依赖列表
     */
    public static Collection<String> convertDependenciesToStringsList(List<CargoDependencyMetadata> dependencies) {
        List<String> dependenciesAsStrings = new ArrayList<>();
        try {
            for (CargoDependencyMetadata dependency : dependencies)
                dependenciesAsStrings.add(MAPPER.writeValueAsString(dependency));
        } catch (JsonProcessingException e) {
            log.warn("Failed to convert dependency to str");
        }
        return dependenciesAsStrings;
    }

    /**
     * 将字符串列表转换为依赖列表
     *
     * @param dependencies 字符串列表，表示依赖列表
     * @return 依赖列表，由CargoDependencyMetadata对象表示
     */
    public static List<CargoDependencyMetadata> convertToDependencies(Collection<String> dependencies) {
        List<CargoDependencyMetadata> ret = new ArrayList<>();
        if (dependencies != null)
            for (String dependency : dependencies) {
                if (StringUtils.isEmpty(dependency))
                    continue;
                String[] split = dependency.split(";");
                Arrays.<String>stream(split).forEach(line -> {
                    try {
                        ret.add((CargoDependencyMetadata) MAPPER.readValue(dependency, CargoDependencyMetadata.class));
                    } catch (JsonProcessingException e) {
                        log.warn("Failed to convert cargo dependency {}", dependency, e);
                    }
                });
            }
        return ret;
    }

    /**
     * 构建 Crate 路径
     *
     * @param name     Crate 名称
     * @param version  Crate 版本
     * @return Crate 路径
     */
    public static String buildCratePath(String name, String version) {
        if (name == null)
            throw new NullPointerException("name is marked non-null but is null");
        if (version == null)
            throw new NullPointerException("version is marked non-null but is null");
        return String.join("/", new CharSequence[]{"crates", name, name + "-" + version + ".crate"});
    }

    /**
     * 将元数据转换为属性映射
     *
     * @param metadata CargoMetadata对象，表示Cargo.toml文件中的元数据信息
     * @return 属性映射，通常是一个字符串
     */
    public static Map<String, String> metadataToAttributesMap(CargoMetadata metadata) {
        Map<String, String> metadataMap = new LinkedHashMap<>();
        try {
            metadataMap.put("crate.name", metadata.getName());
            metadataMap.put("crate.version", metadata.getVers());
            if (StringUtils.isNotBlank(metadata.getDescription()))
                metadataMap.put("crate.description", metadata.getDescription());
            List<String> keywords = metadata.getKeywords();
            if (CollectionUtils.isNotNullOrEmpty(keywords))
                metadataMap.put("crate.keywords", collectionAsString(keywords));
            List<String> categories = metadata.getCategories();
            if (CollectionUtils.isNotNullOrEmpty(categories))
                metadataMap.put("crate.categories", collectionAsString(categories));
        } catch (Exception e) {
            log.error("error during metadata write", e);
        }
        return metadataMap;
    }

    /**
     * 将集合转换为字符串
     *
     * @param collection 集合对象，通常是一个字符串
     * @return 字符串，表示集合
     */
    private static String collectionAsString(Collection<String> collection) {
        return String.join(";", Optional.ofNullable(collection).orElse(List.of()));
    }

    /**
     * 将属性映射转换为元数据
     *
     * @param attributes 属性映射，通常是一个字符串
     * @param readLongMetadataHandler 读取长元数据的处理器，通常是一个字符串
     * @return CargoMetadata对象，表示Cargo.toml文件中的元数据信息
     */
    public static CargoMetadata attributesMapToMetadata(CargoMetadataExtractor cargoMetadataExtractor,RepositoryPath repositoryPath, Supplier<CargoMetadata.CargoLongMetadata> readLongMetadataHandler) {
        if (readLongMetadataHandler == null) {
            throw new NullPointerException("readLongMetadataHandler is marked non-null but is null");
        }
        CargoMetadata metadata =  cargoMetadataExtractor.extract(repositoryPath);
        //metadata.setName(getOnlyElement(attributes.get("crate.name")));
        //metadata.setVers(getOnlyElement(attributes.get("crate.version")));
        //metadata.setDescription(getOnlyElement(attributes.get("crate.description")));
        //String keywords = getOnlyElement(attributes.get("crate.keywords"));
        //if (StringUtils.isNotBlank(keywords)) {
        //    metadata.setKeywords(List.of(Arrays.toString((Object[]) keywords.split(";"))));
        //}
        //String categories = getOnlyElement(attributes.get("crate.categories"));
        //if (StringUtils.isNotBlank(categories)) {
        //    metadata.setCategories(List.of(Arrays.toString((Object[]) categories.split(";"))));
        //}
        CargoMetadata.CargoLongMetadata cargoLongMetadata = getLongMetadata(metadata, readLongMetadataHandler);
        metadata.setCargoLongMetadata(cargoLongMetadata);
        return metadata;
    }

    /**
     * 创建一个多映射
     *
     * @param attributes 属性映射，通常是一个字符串
     * @return 多映射，通常是一个字符串
     */
    public static SetMultimap<String, String> createMultiMap(Map<String, String> attributes) {
        LinkedHashMultimap linkedHashMultimap = LinkedHashMultimap.create();
        for (Map.Entry<String, String> entry : attributes.entrySet())
            linkedHashMultimap.put(entry.getKey(), entry.getValue());
        return (SetMultimap<String, String>) linkedHashMultimap;
    }

    /**
     * 获取唯一元素
     *
     * @param elements 元素集合，通常是一个字符串
     * @return 唯一元素，通常是一个字符串
     */
    public static String getOnlyElement(Collection<String> elements) {
        return (elements == null) ? null : elements.stream().findFirst().orElse(null);
    }


    public static ObjectMapper getMapper() {
        return MAPPER;
    }

    /**
     * 获取长元数据文件路径
     *
     * @param cratePath Crate路径，通常是一个字符串
     * @return 长元数据文件路径，通常是一个字符串
     */
    public static String getLongMetadataFilePath(String cratePath) {
        if (cratePath == null)
            throw new NullPointerException("cratePath is marked non-null but is null");
        return CargoConstants.METADATA_DIRECTORY +"/" + cratePath.replace(CargoConstants.CRATE_SUFFIX, CargoConstants.LONG_METADATA_FILE_SUFFIX);
    }

    /**
     * 写入长元数据
     *
     * @param metadata  CargoMetadata对象，表示Cargo.toml文件中的元数据信息
     */
    public static void writeLongMetadata(CargoMetadata metadata,RepositoryPath repositoryPath, ArtifactManagementService artifactManagementService) throws Exception {
        if (!metadata.getCargoLongMetadata().isEmpty()){
            artifactManagementService.validateAndStore(repositoryPath, new ByteArrayInputStream(
                    JsonUtils.getInstance().valueToByteArray(metadata.getCargoLongMetadata())));
            log.info("Writing long metadata for crate {}", repositoryPath.getTarget().toString());
        }
    }

    public static void writeLongMetadata(CargoMetadata.CargoLongMetadata cargoLongMetadata,RepositoryPath repositoryPath, ArtifactManagementService artifactManagementService) throws Exception {
        if (!cargoLongMetadata.isEmpty()){
            artifactManagementService.validateAndStore(repositoryPath, new ByteArrayInputStream(
                    JsonUtils.getInstance().valueToByteArray(cargoLongMetadata)));
            log.info("Writing long metadata for crate {}", repositoryPath.getTarget().toString());
        }
    }

    private static List<String> convertTomlArrayToList(TomlArray array) {
        List<String> list = new ArrayList<>();
        for (int i = 0; i < array.size(); i++)
            list.add(array.getString(i));
        return list;
    }

    private static ObjectMapper initObjectMapper() {
        ObjectMapper mapper = new ObjectMapper();
        mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL)
                .setPropertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE).enable(MapperFeature.DEFAULT_VIEW_INCLUSION);

        mapper.setVisibility(mapper.getSerializationConfig().getDefaultVisibilityChecker()
                .withFieldVisibility(JsonAutoDetect.Visibility.ANY).withGetterVisibility(JsonAutoDetect.Visibility.NONE)
                .withSetterVisibility(JsonAutoDetect.Visibility.NONE));
        return mapper;
    }

    private static CargoMetadata.CargoLongMetadata getLongMetadata( CargoMetadata metadata, Supplier<CargoMetadata.CargoLongMetadata> readLongMetadataHandler) {
        if (readLongMetadataHandler == null)
            throw new NullPointerException("readLongMetadataHandler is marked non-null but is null");
        CargoMetadata.CargoLongMetadata cargoLongMetadata = readLongMetadataHandler.get();
        if (cargoLongMetadata == null) {
            cargoLongMetadata = new CargoMetadata.CargoLongMetadata();
            log.debug("Trying to resolve dependencies from crate properties");
            //String deps = metadata.getDeps();//getOnlyElement(attributes.get("crate.dependencies"));
            if (metadata.getDeps() != null && !metadata.getDeps().isEmpty())
                cargoLongMetadata.setDeps(metadata.getDeps());
            log.debug("Trying to resolve features from crate properties");
            //String featuresStr = getOnlyElement(metadata.getFeatures());  //getOnlyElement(attributes.get("crate.features"));
            //if (StringUtils.isNotBlank(featuresStr)) {
            //Map<String, List<String>> features = stringToFeatures(featuresStr);
            cargoLongMetadata.setFeatures(metadata.getFeatures());
            //}
        }
        return cargoLongMetadata;
    }

    @Data
    @NoArgsConstructor
    private static class CargoIndexLine {
        String name;

        String vers;

        List<CargoUtil.CargoIndexDependency> deps;

        String cksum;

        Map<String, List<String>> features;

        boolean yanked;

        String link;


        public CargoIndexLine(String name, String vers, List<CargoUtil.CargoIndexDependency> deps, String cksum, Map<String, List<String>> features, boolean yanked, String link) {
            this.name = name;
            this.vers = vers;
            this.deps = deps;
            this.cksum = cksum;
            this.features = features;
            this.yanked = yanked;
            this.link = link;
        }
    }

    @Data
    @NoArgsConstructor
    private static class CargoIndexDependency {
        String name;

        String req;

        List<String> features;

        boolean optional;

        boolean defaultFeatures;

        //@JsonInclude(JsonInclude.Include.ALWAYS)
        @JsonInclude(JsonInclude.Include.NON_NULL)
        String target;

        String kind;

        String registry;

        @JsonProperty("package")
        String packageOriginalName;

        public CargoIndexDependency(String name, String req, List<String> features, boolean optional, boolean defaultFeatures, String target, String kind, String registry, String packageOriginalName) {
            this.name = name;
            this.req = req;
            this.features = features;
            this.optional = optional;
            this.defaultFeatures = defaultFeatures;
            this.target = target;
            this.kind = kind;
            this.registry = registry;
            this.packageOriginalName = packageOriginalName;
        }



        private static CargoIndexDependency toIndexDependency(CargoDependencyMetadata d) {
            if (StringUtils.isNotBlank(d.getPackageExplicitName()))
                return new CargoIndexDependency(d.getPackageExplicitName(), d.getVersionReq(),
                        (d.getFeatures() == null) ? List.of() : d.getFeatures(), d.isOptional(), d
                        .isDefaultFeatures(), d.getTarget(), d.getKind(), d.getRegistry(), d.getName());
            return new CargoIndexDependency(d.getName(), d.getVersionReq(),
                    (d.getFeatures() == null) ? List.of() : d.getFeatures(), d.isOptional(), d
                    .isDefaultFeatures(), d.getTarget(), d.getKind(), d.getRegistry(), null);
        }
    }

    @Getter
    enum CargoDependencyKind {
        NORMAL("normal", "dependencies"),
        BUILD("build", "build-dependencies"),
        DEV("dev", "dev-dependencies");


        CargoDependencyKind(String kind, String type) {
            this.kind = kind;
            this.type = type;
        }

        private final String kind;

        private final String type;


        public static CargoDependencyKind fromType(String type) {
            return Arrays.<CargoDependencyKind>stream(values()).filter(kind -> kind.type.equals(type)).findFirst().orElse(NORMAL);
        }
    }

    public static Integer getSearchPerPage(Integer perPage) {
        if (perPage == null) {
            perPage = DEFAULT_SEARCH_PER_PAGE;
        } else if (perPage.intValue() > MAX_SEARCH_PER_PAGE.intValue()) {
            perPage = MAX_SEARCH_PER_PAGE;
        }
        return perPage;
    }
}

