package com.veadan.folib.gremlin.service.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.metadata.fill.FillConfig;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import com.veadan.folib.gremlin.entity.vo.ArtifactVo;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.gremlin.service.ArtifactService;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.util.LocalDateTimeInstance;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.JanusGraph;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.UndeclaredThrowableException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLEncoder;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Component
@Transactional
public class ArtifactServiceImpl extends GremlinVertexRepository<Artifact> implements ArtifactService {

    @Inject
    private ArtifactAdapter artifactAdapter;

    @Inject
    private ArtifactRepository artifactRepository;

    @Inject
    private JanusGraph janusGraph;


    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }


    @Override
    public void saveOrUpdateArtifact(Artifact artifact) {
        Graph g = janusGraph.tx().createThreadedTx();
        try {
            artifact.setLastUpdated(LocalDateTimeInstance.now());
            merge(() -> g.traversal(EntityTraversalSource.class), artifact);
            g.tx().commit();
        } catch (Throwable e) {
            g.tx().rollback();
            throw new UndeclaredThrowableException(e);
        } finally {
            g.tx().close();
        }
    }

    @Override
    public void exportExcel(String vulnerabilityUuid, String storageId, String repositoryId) throws IOException {
        List<Artifact> artifactList = artifactRepository.findMatchingByVulnerabilityUuid(vulnerabilityUuid, storageId, repositoryId);
        InputStream template = this.getClass().getResourceAsStream("/template/vulnerabilityTemplate.xlsx");
        HttpServletResponse response = ((ServletRequestAttributes) RequestContextHolder.currentRequestAttributes()).getResponse();
        try (ExcelWriter excelWriter = EasyExcel.write(response.getOutputStream()).withTemplate(template).build()) {
            WriteSheet writeSheet = EasyExcel.writerSheet().build();
            FillConfig fillConfig = FillConfig.builder().build();
            Map<String, Object> map = Maps.newHashMap();
            map.put("vulnerabilityID", vulnerabilityUuid);
            excelWriter.fill(map, writeSheet);
            if (CollectionUtils.isNotEmpty(artifactList)) {
                List<List<Artifact>> list = Lists.partition(artifactList, 200);
                for (List<Artifact> itemList : list) {
                    // 放入数据
                    excelWriter.fill(itemList.stream().map(artifact -> {
                        ArtifactVo artifactVo = ArtifactVo.builder().build();
                        BeanUtils.copyProperties(artifact, artifactVo);
                        SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                        if (artifact.getCreated() != null) {
                            String createdTime = DateUtil.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setCreatedTime(createdTime);
                        }
                        if (artifact.getLastUsed() != null) {
                            String lastUsedTime = DateUtil.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setLastUsedTime(lastUsedTime);
                        }
                        if (artifact.getLastUpdated() != null) {
                            String lastModified = DateUtil.format(Date.from(artifact.getLastUpdated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                            artifactVo.setLastModified(lastModified);
                        }
                        artifactVo.setSha(artifact.getChecksums().get("SHA-1"));
                        artifactVo.setMd5(artifact.getChecksums().get("MD5"));
                        artifactVo.setSize(fileSizeConvert(artifact.getSizeInBytes()));
                        artifactVo.setName(artifact.getUuid().substring(artifact.getUuid().lastIndexOf("/") + 1));
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

    private String fileSizeConvert(Long sizeInBytes) {
        BigDecimal bigDecimal = BigDecimal.valueOf(sizeInBytes);
        String size = "";
        double kb = 1024;
        double mb = 1024 * 1024;
        double gb = 1024 * 1024 * 1024;
        double bSize = 0.1 * kb;
        double kbSize = 0.1 * mb;
        double mbSize = 0.1 * gb;
        if (sizeInBytes < bSize) {
            //如果小于0.1KB转化成B
            size = bigDecimal.setScale(2, RoundingMode.HALF_UP) + "B";
        } else if (sizeInBytes < kbSize) {
            //如果小于0.1MB转化成KB
            size = bigDecimal.divide(BigDecimal.valueOf(kb), 2, RoundingMode.HALF_UP) + "KB";
        } else if (sizeInBytes < mbSize) {
            //如果小于0.1GB转化成MB
            size = bigDecimal.divide(BigDecimal.valueOf(mb), 2, RoundingMode.HALF_UP) + "MB";
        } else {
            //其他转化成GB
            size = bigDecimal.divide(BigDecimal.valueOf(gb), 2, RoundingMode.HALF_UP) + "GB";
        }
        return size;
    }


    @Override
    public void exportPdf(String vulnerabilityUuid, String storageId, String repositoryId) {

    }
}
