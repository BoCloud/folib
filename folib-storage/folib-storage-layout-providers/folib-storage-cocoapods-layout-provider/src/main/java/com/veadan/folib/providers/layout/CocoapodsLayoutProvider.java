package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.CocoapodsRepositoryFeatures;
import com.veadan.folib.repository.CocoapodsRepositoryManagementStrategy;
import com.veadan.folib.repository.RepositoryManagementStrategy;
import com.veadan.folib.util.CocoapodsArtifactUtil;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.util.Set;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 15:09
 * @since x.x.x
 */
@Component
public class CocoapodsLayoutProvider extends AbstractLayoutProvider<CocoapodsArtifactCoordinates>
{
    private static final Logger logger = LoggerFactory.getLogger(CocoapodsLayoutProvider.class);
    
    
    public static final String ALIAS = CocoapodsArtifactCoordinates.LAYOUT_NAME;
    
    @Inject
    private CocoapodsRepositoryManagementStrategy cocoapodsRepositoryManagementStrategy;
    
    @Inject
    private CocoapodsRepositoryFeatures cocoapodsRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                getClass().getCanonicalName(), ALIAS );
    }
    
    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return cocoapodsRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return cocoapodsRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias() {
        return ALIAS;
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    protected CocoapodsArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException 
    {
        final String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
        final String tarGzFilePath = repositoryPath.getTarget().toString();

        CocoapodsArtifactCoordinates coordinates = new CocoapodsArtifactCoordinates(relativizePath);

        if (null != repositoryPath.getArtifactEntry())
        {
            final CocoapodsArtifactCoordinates artifactCoordinates = (CocoapodsArtifactCoordinates) repositoryPath.getArtifactEntry().getArtifactCoordinates();
            final String path = artifactCoordinates.getPath();
            final String version = artifactCoordinates.getVersion();
            final String baseName = artifactCoordinates.getBaseName();
            if (StringUtils.isNotBlank(path))
            { coordinates.setPath(path); }
            if (StringUtils.isNotBlank(version))
            { coordinates.setVersion(version); }
            if (StringUtils.isNotBlank(baseName))
            { coordinates.setBaseName(baseName); }
        }
        if (relativizePath.endsWith("tar.gz"))
        {
            if (StringUtils.isEmpty(coordinates.getBaseName()) || StringUtils.isEmpty(coordinates.getVersion())) 
            { // 如发现制品信息不全，尝试从制品包里读取数据
                final CocoapodsArtifactUtil.PodSpec podSpec = CocoapodsArtifactUtil.resolvePodSpecByTarGzFile(tarGzFilePath);
                if (null != podSpec)
                {
                    coordinates.setBaseName(podSpec.getName());
                    coordinates.setVersion(podSpec.getVersion());
                }
            }
        }
        
        return coordinates;
    }
}
