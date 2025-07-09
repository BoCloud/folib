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
package com.folib.services;

import com.folib.domain.Artifact;
import com.folib.domain.ArtifactStatistics;
import com.folib.domain.DockeerImageResult;
import com.folib.domain.StatusInfo;
import com.folib.domain.thirdparty.ArtifactInfo;
import com.folib.domain.thirdparty.ArtifactQuery;

import com.folib.forms.artifact.ArtifactMetadataForm;
import com.folib.forms.scanner.*;
import com.folib.providers.io.RepositoryPath;
import com.folib.scanner.common.msg.TableResultResponse;
import org.springframework.security.core.Authentication;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * @author veadan
 * @date 2022/10/8
 **/
public interface ArtifactWebService {

    /**
     * 导出受漏洞影响的制品信息
     *
     * @param vulnerabilityUuid 漏洞id
     * @param storageId         存储空间id
     * @param repositoryId      仓库id
     * @param response          响应流
     * @throws IOException 异常
     */
    void exportExcel(String vulnerabilityUuid,
                     String storageId,
                     String repositoryId, HttpServletResponse response) throws IOException;

    /**
     * 查询受漏洞影响的制品信息
     *
     * @param pageNumber        页码
     * @param pageSize          每页数量
     * @param vulnerabilityUuid 漏洞id
     * @param storageId         存储空间id
     * @param repositoryId      仓库id
     * @param artifactName      artifactName
     * @return 制品列表
     */
    TableResultResponse<com.folib.domain.ArtifactInfo> getArtifacts(Integer pageNumber, Integer pageSize, String vulnerabilityUuid,
                                                                    String storageId,
                                                                    String repositoryId, String artifactName);

    /**
     * 全局设置添加或者更新元数据
     *
     * @param artifactMetadataForm 参数
     * @throws IOException 异常
     */
    void globalSettingAddOrUpdateMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException;

    /**
     * 全局设置删除元数据
     *
     * @param artifactMetadataForm 参数
     * @throws IOException 异常
     */
    void globalSettingDeleteMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException;

    /**
     * 获取全局设置的元数据
     *
     * @return 全局设置的元数据
     */
    List<ArtifactMetadataForm> getMetadataConfiguration();

    /**
     * 新增制品元数据
     *
     * @param artifactMetadataForm 参数
     * @return 结果
     */
    String saveArtifactMetadata(ArtifactMetadataForm artifactMetadataForm);

    /**
     * 修改制品元数据
     *
     * @param artifactMetadataForm 参数
     * @return 结果
     */
    String updateArtifactMetadata(ArtifactMetadataForm artifactMetadataForm);

    /**
     * 删除制品元数据
     *
     * @param artifactMetadataForm 参数
     */
    void deleteArtifactMetadata(ArtifactMetadataForm artifactMetadataForm);

    /**
     * 扫描信息统计
     *
     * @param authentication 登录用户
     * @return 扫描信息统计
     */
    CountForm getCount(Authentication authentication);

    /**
     * 近一个月内统计信息
     *
     * @param authentication 登录用户
     * @return 近一个月内统计信息
     */
    List<DayCountForm> monthCount(Authentication authentication);

    /**
     * 近一周内数据
     *
     * @param authentication 登录用户
     * @return 近一周内数据
     */
    WeekCountForm weekCount(Authentication authentication);

    /**
     * 仓库扫描情况
     *
     * @param authentication 登录用户
     * @return 仓库扫描情况
     */
    List<RepositoryCountForm> repositories(Authentication authentication);

    /**
     * 仓库扫描情况
     *
     * @param storageId    存储空间id
     * @param repositoryId 仓库id
     * @param artifactName 搜索词
     * @param page         页码
     * @param limit        每页数量
     * @return 仓库扫描情况
     */
    RepositoryScannerForm repository(String storageId, String repositoryId, String artifactName, Integer page, Integer limit);

    /**
     * 批量存储或更新元数据
     *
     * @param artifactMetadataFormList artifactMetadataFormList
     */
    void batchArtifactMetadata(List<ArtifactMetadataForm> artifactMetadataFormList);

    /***
     * 获取制品信息
     * @param repositoryPath 路径
     * @return 制品信息
     * @throws Exception 异常
     */
    Artifact getArtifact(RepositoryPath repositoryPath) throws Exception;


    /**
     * 强制生成图数据库信息，已存在图库中继续更新
     *
     * @param username                  用户名
     * @param beginDate                 开始日期
     * @param endDate                   结束日期
     * @param storageId                 存储空间
     * @param repositoryId              仓库id
     * @param storageIdAndRepositoryIds 存储空间-仓库组合
     * @param path                      path
     * @param metadata                  是否同步元数据 true 是 false 否
     * @param batch                     每批数量
     */
    void buildGraphIndexForce(String username, String beginDate, String endDate, String storageId, String repositoryId, String storageIdAndRepositoryIds, String path, Boolean metadata, Integer batch);

    /**
     * 根据压缩包生成制品信息
     *
     * @param username     用户名
     * @param storageId    存储空间
     * @param repositoryId 仓库id
     * @param path         path
     * @param uuid         uuid
     * @param file         制品压缩文件
     * @return statusInfo 状态
     */
    StatusInfo store(String username, String storageId, String repositoryId, String path, String uuid, MultipartFile file);

    /**
     * 制品统计信息
     *
     * @return 制品统计信息
     */
    ArtifactStatistics artifactStatistics();

    /**
     * 查询制品分页列表
     *
     * @param artifactQuery 查询参数
     * @return 制品分页列表
     */
    TableResultResponse<ArtifactInfo> thirdPartyPage(ArtifactQuery artifactQuery);

    /**
     * 清理仓库-快速
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库
     * @param deleteFile   true 删除文件 其他不删除
     * @param limit        批处理数量
     */
    void cleanupRepository(String storageId, String repositoryId, Boolean deleteFile, Integer limit);

    /**
     * 制品预览
     *
     * @param repositoryPath 制品路径
     * @return 预览信息
     */
    List preview(RepositoryPath repositoryPath);

    /**
     * 制品扫描
     *
     * @param repositoryPath 制品路径
     */
    void scan(RepositoryPath repositoryPath);

    /**
     * 生成Java Heap Dump
     *
     * @param filePath 保存路径
     * @return 保存路径
     */
    String dumpHead(String filePath);

    /**
     * 上传制品的bom文件
     *
     * @param repositoryPath 制品信息
     * @param file           bom文件
     */
    void bomUpload(RepositoryPath repositoryPath, MultipartFile file);

    /**
     * foeyes是否可用
     *
     * @return true 可用 false 不可用
     */
    boolean foEyesEnable();

    /**
     * docker仓库旧版本升级
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @param override     是否覆盖 true 是 false 否
     * @throws Exception 异常
     */
    void dockerLayoutUpgrade(String storageId, String repositoryId, Boolean override) throws Exception;

    /**
     * docker完整性校验
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @throws Exception 异常
     */
    void dockerIntegrity(String storageId, String repositoryId) throws Exception;

    /**
     * docker仓库新版本降级
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @throws Exception 异常
     */
    void dockerLayoutDowngrade(String storageId, String repositoryId) throws Exception;

    /**
     * docker仓库旧版本升级
     *
     * @throws Exception 异常
     */
    void dockerLayoutUpgradeAll() throws Exception;

    /**
     * 删除ARTIFACTS_RESOLVE权限
     *
     * @param roleId     角色id
     * @param resourceId 资源id
     */
    void deleteArtifactsResolve(String roleId, String resourceId);

    void saveArtifactMetaByString(String storageId, String repositoryId, String path, String metaData);

    /**
     * 根据uuid前缀统计数量
     *
     * @param uuidPrefix uuid前缀
     * @return 数量
     */
    long countByUUidPrefix(String uuidPrefix);

    /**
     * 获取元数据
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @param path         路径
     * @return 元数据
     */
    String getMetadata(String storageId, String repositoryId, String path);

    /**
     * 处理缓存元数据
     *
     * @param artifactPath   制品路径
     * @param repositoryPath 制品信息
     * @return 元数据信息
     */
    String handlerMetadata(String artifactPath, RepositoryPath repositoryPath);

    /**
     * 强制删除制品信息
     *
     * @param repositoryPath 制品信息
     */
    void doForceDelete(RepositoryPath repositoryPath);

    /**
     * 查询docker镜像信息
     *
     * @param imageNumber 镜像数量
     * @param imageSize 镜像大小
     * @return 镜像信息
     */
    List<DockeerImageResult> queryDockerImages(Integer imageNumber,long imageSize);
}
