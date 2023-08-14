package com.veadan.folib.components.promotion;

import com.veadan.folib.configuration.UnionTargetRepositoryConfiguration;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.providers.io.RepositoryPath;

/**
 * @author leipenghui
 **/
public interface ArtifactPromotionProvider {

    /**
     * 注册
     */
    void register();

    /**
     * 联邦仓库制品晋级
     *
     * @param repositoryPath                     需要晋级的源制品
     * @param artifactPath                       需要晋级的源制品路径
     * @param unionTargetRepositoryConfiguration 要晋级到的目标仓库信息
     */
    void promotion(RepositoryPath repositoryPath, String artifactPath, UnionTargetRepositoryConfiguration unionTargetRepositoryConfiguration);

    /**
     * 制品分发
     *
     * @param artifactDispatch 要分发到的目标仓库信息
     */
    void dispatch(ArtifactDispatch artifactDispatch);
}
