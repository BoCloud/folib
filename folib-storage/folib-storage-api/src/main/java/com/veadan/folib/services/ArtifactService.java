package com.veadan.folib.services;

import com.veadan.folib.domain.Artifact;

import java.io.IOException;
import java.util.List;

/**
 * @author leipenghui
 * @date 2022/10/8
 **/
public interface ArtifactService {

    /**
     * 保存或者更新制品顶点
     *
     * @param artifact 制品
     */
    void saveOrUpdateArtifact(Artifact artifact);
}
