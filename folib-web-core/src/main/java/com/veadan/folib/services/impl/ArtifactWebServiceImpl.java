package com.veadan.folib.services.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.configuration.MutableMetadataConfiguration;
import com.veadan.folib.controllers.ResponseMessage;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactMetadata;
import com.veadan.folib.forms.artifact.ArtifactMetadataForm;
import com.veadan.folib.gremlin.entity.vo.ArtifactVo;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.services.ArtifactService;
import com.veadan.folib.services.ArtifactWebService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.FileSizeConvertUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@Transactional
public class ArtifactWebServiceImpl implements ArtifactWebService {

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private ArtifactService artifactService;

    @Inject
    private ArtifactResolutionService artifactResolutionService;

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Override
    public void exportExcel(String vulnerabilityUuid, String storageId, String repositoryId, HttpServletResponse response) throws IOException {
        List<Artifact> artifactList = artifactRepository.findMatchingByVulnerabilityUuid(vulnerabilityUuid, storageId, repositoryId);
        InputStream template = this.getClass().getResourceAsStream("/template/vulnerabilityTemplate.xlsx");
        try (ExcelWriter excelWriter = EasyExcel.write(response.getOutputStream()).withTemplate(template).build()) {
            WriteSheet writeSheet = EasyExcel.writerSheet().build();
            FillConfig fillConfig = FillConfig.builder().build();
            Map<String, Object> map = Maps.newHashMap();
            map.put("vulnerabilityID", vulnerabilityUuid);
            excelWriter.fill(map, writeSheet);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                SimpleDateFormat df = DateUtil.newSimpleFormat("yyyy-MM-dd HH:mm:ss");
                List<List<Artifact>> list = Lists.partition(artifactList, 200);
                for (List<Artifact> itemList : list) {
                    // 放入数据
                    excelWriter.fill(itemList.stream().map(artifact -> {
                        ArtifactVo artifactVo = ArtifactVo.builder().build();
                        BeanUtils.copyProperties(artifact, artifactVo);
                        if (artifact.getCreated() != null) {
                            String createdTime = DateUtil.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setCreatedTime(createdTime);
                        }
                        if (artifact.getLastUsed() != null) {
                            String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setLastUsedTime(lastUsedTime);
                        }
                        artifactVo.setSha(artifact.getChecksums().get("SHA-1"));
                        artifactVo.setMd5(artifact.getChecksums().get("MD5"));
                        artifactVo.setSize(FileSizeConvertUtils.convert(artifact.getSizeInBytes()));
                        artifactVo.setName(artifact.getUuid().substring(artifact.getUuid().lastIndexOf("/") + 1));
                        if (StringUtils.isNotBlank(artifact.getStorageId()) && StringUtils.isNotBlank(artifact.getRepositoryId())) {
                            Repository repository = configurationManagementService.getConfiguration().getRepository(artifact.getStorageId(), artifact.getRepositoryId());
                            if (Objects.nonNull(repository) && "Docker".equalsIgnoreCase(repository.getLayout())) {
                                String path = artifact.getArtifactPath();
                                //docker
                                artifactVo.setName(path.substring(0, path.indexOf("/blobs/sha256")));
                            }
                        }
                        return artifactVo;
                    }).collect(Collectors.toList()), fillConfig, writeSheet);
                }
            }
            // 设置响应头
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
            response.setCharacterEncoding("utf-8");
            // 这里URLEncoder.encode可以防止中文乱码
            String fileName = URLEncoder.encode(vulnerabilityUuid + "影响范围", "utf-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + fileName + ".xlsx");
            excelWriter.finish();
        }
    }

    @Override
    public void globalSettingAddOrUpdateMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException {
        MutableMetadataConfiguration mutableMetadataConfiguration = MutableMetadataConfiguration.builder().build();
        BeanUtils.copyProperties(artifactMetadataForm, mutableMetadataConfiguration);
        configurationManagementService.addOrUpdateMetadataConfiguration(mutableMetadataConfiguration);
    }

    @Override
    public void globalSettingDeleteMetadata(ArtifactMetadataForm artifactMetadataForm) throws IOException {
        configurationManagementService.deleteMetadataConfig(artifactMetadataForm.getKey());
    }

    @Override
    public List<ArtifactMetadataForm> getMetadataConfiguration() {
        return Optional.of(configurationManagementService.getConfiguration().getMetadataConfiguration().values().stream().collect(Collectors.toCollection(LinkedList::new))).orElse(Lists.newLinkedList()).stream().map(item -> {
            ArtifactMetadataForm artifactMetadata = ArtifactMetadataForm.builder().build();
            BeanUtils.copyProperties(item, artifactMetadata);
            return artifactMetadata;
        }).collect(Collectors.toList());
    }

    @Override
    public String saveArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        try {
            RepositoryPath resolvePath = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            Artifact artifact = resolvePath.getArtifactEntry();
            JSONObject metadataJson = getMetadata(artifact);
            if (Objects.isNull(metadataJson)) {
                metadataJson = new JSONObject();
            }
            String key = artifactMetadataForm.getKey();
            if (metadataJson.containsKey(key)) {
                //已存在
                return "repeat";
            }
            ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
            BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
            metadataJson.put(key, artifactMetadata);
            artifact.setMetadata(metadataJson.toJSONString());
            artifactService.saveOrUpdateArtifact(artifact);
        } catch (Exception ex) {
            log.error("=====>>>>>保存制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("保存制品元数据错误，请稍后重试");
        }
        return ResponseMessage.ok().getMessage();
    }

    @Override
    public String updateArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        try {
            RepositoryPath resolvePath = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            Artifact artifact = resolvePath.getArtifactEntry();
            JSONObject metadataJson = getMetadata(artifact);
            String key = artifactMetadataForm.getKey();
            if (Objects.nonNull(metadataJson) && metadataJson.containsKey(key)) {
                ArtifactMetadata artifactMetadata = ArtifactMetadata.builder().build();
                BeanUtils.copyProperties(artifactMetadataForm, artifactMetadata);
                metadataJson.put(key, artifactMetadata);
                artifact.setMetadata(metadataJson.toJSONString());
                artifactService.saveOrUpdateArtifact(artifact);
            }
        } catch (Exception ex) {
            log.error("=====>>>>>修改制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("修改制品元数据错误，请稍后重试");
        }
        return ResponseMessage.ok().getMessage();
    }

    @Override
    public void deleteArtifactMetadata(ArtifactMetadataForm artifactMetadataForm) {
        try {
            RepositoryPath resolvePath = resolvePath(artifactMetadataForm.getStorageId(), artifactMetadataForm.getRepositoryId(), artifactMetadataForm.getArtifactPath());
            Artifact artifact = resolvePath.getArtifactEntry();
            JSONObject metadataJson = getMetadata(artifact);
            if (Objects.nonNull(metadataJson) && metadataJson.containsKey(artifactMetadataForm.getKey())) {
                metadataJson.remove(artifactMetadataForm.getKey());
                artifact.setMetadata(metadataJson.toJSONString());
                artifactService.saveOrUpdateArtifact(artifact);
            }
        } catch (Exception ex) {
            log.error("=====>>>>>删除制品元数据错误：{}", ExceptionUtils.getStackTrace(ex));
            throw new RuntimeException("删除制品元数据错误，请稍后重试");
        }
    }

    /**
     * 获取制品元数据
     *
     * @param artifact artifact
     * @return 制品元数据
     * @throws IOException 异常
     */
    private JSONObject getMetadata(Artifact artifact) throws IOException {
        String metadata = artifact.getMetadata();
        JSONObject metadataJson = null;
        if (StringUtils.isNotBlank(metadata)) {
            metadataJson = JSONObject.parseObject(metadata);
        }
        return metadataJson;
    }

    /***
     * 获取制品RepositoryPath
     * @param storageId 存储空间名称
     * @param repositoryId 仓库名称
     * @param artifactPath 制品路径
     * @return RepositoryPath
     * @throws Exception 异常
     */
    private RepositoryPath resolvePath(String storageId, String repositoryId, String artifactPath) throws Exception {
        return artifactResolutionService.resolvePath(storageId, repositoryId, artifactPath);
    }
}
