package com.veadan.folib.providers.layout;

import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repository.RepositoryManagementStrategy;

import java.io.IOException;
import java.util.Set;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 15:09
 * @since x.x.x
 */
public class CocoapodsLayoutProvider extends AbstractLayoutProvider<CocoapodsArtifactCoordinates>
{
    public static final String ALIAS = CocoapodsArtifactCoordinates.LAYOUT_NAME;
    
    @Override
    public RepositoryManagementStrategy getRepositoryManagementStrategy() {
        return null;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return null;
    }

    @Override
    public String getAlias() {
        return null;
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath repositoryPath) {
        return false;
    }

    @Override
    protected CocoapodsArtifactCoordinates getArtifactCoordinates(RepositoryPath repositoryPath) throws IOException {
        return null;
    }
}
