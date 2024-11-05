package com.veadan.folib.services;

import com.veadan.folib.providers.io.RepositoryPath;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.model.File;
import org.jfrog.artifactory.client.model.LightweightRepository;

import java.util.List;

/**
 * @author leipenghui
 **/
public interface JFrogService {

    /**
     * 获取Artifactory
     *
     * @param address  地址
     * @param username 用户名
     * @param password 密码
     * @return Artifactory
     */
    Artifactory getArtifactory(String address, String username, String password);

    /**
     * 校验JFrog
     *
     * @param address  地址
     * @param username 用户名
     * @param password 密码
     */
    void validateArtifactory(String address, String username, String password);

    /**
     * 获取仓库列表
     *
     * @param address  地址
     * @param username 用户名
     * @param password 密码
     * @return 仓库列表
     */
    List<LightweightRepository> listRepository(String address, String username, String password);

    /**
     * 获取仓库列表
     *
     * @param address     地址
     * @param username    用户名
     * @param password    密码
     * @param packageTypes 仓库类型
     * @return 仓库列表
     */
    List<LightweightRepository> listRepository(String address, String username, String password, List<String> packageTypes);

    /**
     * 判断仓库是否存在
     *
     * @param repositoryName 仓库名称
     * @param artifactory    artifactory
     * @return true 存在 false 不存在
     */
    boolean existsRepository(String repositoryName, Artifactory artifactory);

    /**
     * 上传制品
     *
     * @param nodeName       目标节点
     * @param repositoryName 目标仓库
     * @param repositoryPath 制品
     * @param artifactPath   路径
     * @param recordStatus   是否记录状态 true（记录）其他（不记录）
     */
    void uploadItem(String nodeName, String repositoryName, RepositoryPath repositoryPath, String artifactPath, Boolean recordStatus);
}
