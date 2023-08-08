package com.veadan.folib.services;

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
     * @param userName 用户名
     * @param password 密码
     * @return Artifactory
     */
    Artifactory getArtifactory(String address, String userName, String password);

    /**
     * 校验JFrog
     *
     * @param address  地址
     * @param userName 用户名
     * @param password 密码
     */
    void validateArtifactory(String address, String userName, String password);

    /**
     * 获取仓库列表
     *
     * @param address  地址
     * @param userName 用户名
     * @param password 密码
     * @return 仓库列表
     */
    List<LightweightRepository> listRepository(String address, String userName, String password);

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
     * @param param 参数
     * @param file  文件
     * @param path  路径
     * @return 结果
     */
    File uploadItem(Object param, java.io.File file, String path);
}
