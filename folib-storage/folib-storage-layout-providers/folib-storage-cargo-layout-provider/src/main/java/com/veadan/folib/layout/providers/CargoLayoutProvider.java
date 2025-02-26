package com.veadan.folib.layout.providers;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.veadan.folib.artifact.coordinates.CargoArtifactCoordinates;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.AbstractLayoutProvider;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.repository.CargoRepositoryFeatures;
import com.veadan.folib.storage.repository.CargoRepositoryManagementStrategy;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.utils.CargoConstants;
import com.veadan.folib.utils.CargoUtil;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Component("cargoLayoutProvider")
public class CargoLayoutProvider extends AbstractLayoutProvider<CargoArtifactCoordinates> {

    private static final Logger logger = LoggerFactory.getLogger(CargoLayoutProvider.class);
    public  static final String ALIAS = CargoArtifactCoordinates.LAYOUT_NAME;
    @Lazy
    @Inject
    private CargoRepositoryManagementStrategy cargoRepositoryManagementStrategy;
    @Lazy
    @Inject
    private CargoRepositoryFeatures cargoRepositoryFeatures;
    @Lazy
    @Inject
    protected ArtifactManagementService artifactManagementService;
    @Lazy
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Lazy
    @Inject
    private ConfigurationManager configurationManager;

    @Lazy
    @Inject
    protected ArtifactResolutionService artifactResolutionService;

    @PostConstruct
    public void register() {
        logger.info("Registered Layout provider '{}' with alias '{}.'",
                getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return cargoRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        if (repositoryPath.getPath().endsWith(CargoConstants.CRATE_SUFFIX) || repositoryPath.getPath().startsWith("index")) {
            return false;
        }
        return true;
    }

    private boolean isArtifact(RepositoryPath repositoryPath) {
        if (repositoryPath.getPath().endsWith(CargoConstants.CRATE_SUFFIX) || repositoryPath.getPath().startsWith("crates")) {
            return true;
        }
        return false;
    }

    private boolean isIndex(RepositoryPath repositoryPath) {
        if (repositoryPath.getPath().startsWith("index")) {
            return true;
        }
        return false;
    }

    @Override
    public CargoArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return new CargoArtifactCoordinates(RepositoryFiles.relativizePath(repositoryPath));
    }

    @Override
    public CargoRepositoryManagementStrategy getRepositoryManagementStrategy() {
        return cargoRepositoryManagementStrategy;
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath, RepositoryFileAttributeType... attributeTypes) throws IOException {
        Map<RepositoryFileAttributeType, Object> result =  super.getRepositoryFileAttributes(repositoryPath, attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes) {
            Object value = result.get(attributeType);
            switch (attributeType) {
                case ARTIFACT:
                    value = (Boolean) value && isArtifact(repositoryPath);
                    if (value != null) {
                        result.put(attributeType, value);
                    }
                    break;
                case METADATA:
                    value = (Boolean) value && isArtifactMetadata(repositoryPath);
                    if (value != null) {
                        result.put(attributeType, value);
                    }
                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (!RepositoryTypeEnum.HOSTED.getType().equals(repositoryPath.getRepository().getType()) && isIndex(repositoryPath))
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo);

                    result.put(attributeType, value);

                    break;
            }
        }
        return result;
    }

    @Override
    public void targetUrl(RepositoryPath path) throws IOException {
        if (Objects.isNull(path) || StringUtils.isBlank(path.getTargetUrl())) {
            return;
        }
        Repository repository = path.getRepository();
        if (!RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            return;
        }
        String remoteUrl = null;
        if(path.getTargetUrl().endsWith("/download")){
            RepositoryPath repositoryPath = artifactResolutionService.resolvePath(path.getStorageId(), path.getRepositoryId(), ".proxy-config.json");
            JSONObject jsonObject = CargoUtil.getMapper().readValue(Files.readString(repositoryPath), JSONObject.class);
            remoteUrl = jsonObject.getString("dl");
        }else {
            remoteUrl = repository.getRemoteRepository().getUrl();
        }
        remoteUrl = StringUtils.removeEnd(remoteUrl, GlobalConstants.SEPARATOR);
        path.setTargetUrl(String.format("%s/%s", remoteUrl, StringUtils.removeStart(path.getTargetUrl(), GlobalConstants.SEPARATOR)));
    }

    @Override
    public void initData(String storageId, String repositoryId) {
        try {
            Repository repository = configurationManager.getConfiguration().getStorage(storageId).getRepository(repositoryId);
            if(repository.isProxyRepository()){
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, ".proxy-config.json");
                if (Files.exists(repositoryPath)) {
                    Files.deleteIfExists(repositoryPath);
                }
                repositoryPath.setTargetUrl("config.json");
                try {
                    repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository.getStorage().getId(), repository.getId(), "config.json");
            if (Files.exists(repositoryPath)) {
                Files.deleteIfExists(repositoryPath);
            }
            String api = getRepositoryBaseUrl(repository);
            ObjectMapper objectMapper = new ObjectMapper();
            Map<String, String> map = new HashMap<>();
            map.put("dl", String.format("%s/api/v1/crates", api));
            map.put("api", api);
            if (!repository.isAllowAnonymous()) {
                map.put("auth-required", "true");
            }
            objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
            ObjectWriter writer = objectMapper.writerWithDefaultPrettyPrinter();
            //repositoryPath = artifactResolutionService.resolvePath(repositoryPath);
            artifactManagementService.store(repositoryPath, new ByteArrayInputStream(writer.writeValueAsString(map).getBytes()));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/artifactory/api/cargo/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getId());
    }


}
