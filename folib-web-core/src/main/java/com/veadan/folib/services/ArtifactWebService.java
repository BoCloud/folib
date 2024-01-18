package com.veadan.folib.services;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactStatistics;
import com.veadan.folib.domain.StatusInfo;
import com.veadan.folib.domain.thirdparty.ArtifactInfo;
import com.veadan.folib.domain.thirdparty.ArtifactQuery;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.forms.scanner.*;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import org.springframework.security.core.Authentication;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

/**
 * @author leipenghui
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
     * 生成图数据库信息
     *
     * @param username     用户名
     * @param storageId    存储空间
     * @param repositoryId 仓库id
     * @param path         path
     * @param metadata     是否同步元数据 true 是 false 否
     * @param batch        每批数量
     */
    void buildGraphIndex(String username, String storageId, String repositoryId, String path, Boolean metadata, Integer batch);

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
     * 清理snapshot遗留数据
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @param path         路径
     */
    void cleanSnapshot(String storageId, String repositoryId, String path);
}
