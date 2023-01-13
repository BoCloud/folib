package com.veadan.folib.services;

import com.veadan.folib.forms.scanner.*;
import com.veadan.folib.configuration.MetadataConfiguration;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.providers.io.RepositoryPath;

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
     * @param username 登录用户
     * @return 扫描信息统计
     */
    CountForm getCount(String username);

    /**
     * 近一个月内统计信息
     *
     * @param username 登录用户
     * @return 近一个月内统计信息
     */
    List<DayCountForm> monthCount(String username);

    /**
     * 近一周内数据
     *
     * @param username 登录用户
     * @return 近一周内数据
     */
    WeekCountForm weekCount(String username);

    /**
     * 仓库扫描情况
     *
     * @param username 登录用户
     * @return 仓库扫描情况
     */
    List<RepositoryCountForm> repositories(String username);

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


    Artifact getArtifact(RepositoryPath repositoryPath) throws Exception ;
}
