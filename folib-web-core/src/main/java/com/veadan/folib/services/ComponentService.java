package com.veadan.folib.services;

import com.veadan.folib.dto.component.ArtifactGraphDto;
import com.veadan.folib.dto.component.ArtifactStatisticsDto;
import com.veadan.folib.dto.component.ComponentTableDto;
import com.veadan.folib.dto.vulnerability.AffectedArtifactsDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;

/**
 * @author veadan
 * @date 2023/5/24
 **/
public interface ComponentService {

    /**
     * 分页查询组件列表
     *
     * @param page          页码
     * @param limit         每页数量
     * @param name          组件名称
     * @param groupId       组名称
     * @param version       版本号
     * @param searchKeyword 搜索关键词
     * @return 组件列表
     */
    TableResultResponse<ComponentTableDto> queryComponentPage(Integer page, Integer limit, String name, String groupId, String version, String searchKeyword);

    /**
     * 根据制品分页查询组件列表
     *
     * @param page          页码
     * @param limit         每页数量
     * @param artifactPath  制品uuid
     * @param searchKeyword 搜索关键词
     * @return 组件列表
     */
    TableResultResponse<ComponentTableDto> queryComponentPageByArtifact(Integer page, Integer limit, String artifactPath, String searchKeyword);

    /**
     * 查询组件信息
     *
     * @param uuid uuid
     * @return 组件信息
     */
    ComponentTableDto queryComponentOne(String uuid);

    /**
     * 根据组件id分页查询关联制品
     *
     * @param page          页码
     * @param limit         每页数量
     * @param componentUuid 组件id
     * @param searchKeyword 搜索关键词
     * @return 制品列表
     */
    TableResultResponse<AffectedArtifactsDto> queryArtifactByComponentUuid(Integer page, Integer limit, String componentUuid, String searchKeyword);

    /**
     * 组件关联的制品图谱展示
     *
     * @param componentUuid 组件id
     * @return 数据
     */
    ArtifactGraphDto artifactGraph(String componentUuid);

    /**
     * 组件关联的制品统计数据
     *
     * @param componentUuid 组件id
     * @return 数据
     */
    ArtifactStatisticsDto artifactStatistics(String componentUuid);
}
