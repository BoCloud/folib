package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.math.MathUtil;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.entity.ExternalNode;
import com.veadan.folib.enums.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import com.veadan.folib.model.request.ArtifactSyncRecordPageReq;
import com.veadan.folib.model.response.ArtifactSyncRecordPageRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ArtifactSyncRecordService;
import io.micrometer.core.instrument.util.StringUtils;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/12/6 14:16
 * @since x.x.x
 */
@Service
public class ArtifactSyncRecordServiceImpl implements ArtifactSyncRecordService {
    
    @Inject
    private ArtifactSyncRecordMapper artifactSyncRecordMapper;
    @Inject
    private ArtifactSyncSlaveRecordMapper artifactSyncSlaveRecordMapper;
    
    
    @Override
    public TableResultResponse<ArtifactSyncRecordPageRes> page(ArtifactSyncRecordPageReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final Integer pageNumber = model.getPageNumber();
        final Integer pageSize = model.getPageSize();
        final Page<Object> page = PageHelper.startPage(pageNumber, pageSize);
        final Example recordExample = Example.builder(ArtifactSyncRecord.class).build();
        recordExample.setOrderByClause("create_time desc");
        if (StringUtils.isNotEmpty(storageId) && StringUtils.isNotEmpty(repositoryId)) {
            recordExample.and().andLike("sourcePath", storageId + "/" + repositoryId + "/%");
        }
        
        final List<ArtifactSyncRecordPageRes> pageResult = Optional.ofNullable(artifactSyncRecordMapper.selectByExample(recordExample))
                .filter(CollUtil::isNotEmpty)
                .orElse(Collections.emptyList())
                .stream().map(ArtifactSyncRecordPageRes::new)
                .collect(Collectors.toList());


        final List<String> syncNoList = pageResult.stream().map(ArtifactSyncRecordPageRes::getSyncNo).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(syncNoList)) {
            final Example slaveRecordExample = Example.builder(ArtifactSyncSlaveRecord.class).build();
            slaveRecordExample.and().andIn("syncNo", syncNoList);
            final List<ArtifactSyncSlaveRecord> artifactSyncSlaveRecordList = Optional.ofNullable(artifactSyncSlaveRecordMapper.selectByExample(slaveRecordExample))
                    .orElse(Collections.emptyList());
            final Map<String, Map<Integer, Long>> groupSyncNoSlaveRecordCountMap = artifactSyncSlaveRecordList.stream()
                    .collect(Collectors.groupingBy(ArtifactSyncSlaveRecord::getSyncNo, 
                            Collectors.groupingBy(ArtifactSyncSlaveRecord::getStatus, Collectors.counting())));

            pageResult.forEach(e -> {
                final Map<Integer, Long> groupStatusMap = groupSyncNoSlaveRecordCountMap.getOrDefault(e.getSyncNo(), Collections.emptyMap());
                final int sumCount = groupStatusMap.values().stream().mapToInt(Long::intValue).sum();
                final Long successCount = groupStatusMap.getOrDefault(ArtifactSyncRecordStatusEnum.SUCCESS.getVal(), 0L);
                e.setSyncProgress(0D);
                if (successCount > 0) {
                    final double syncProgress = BigDecimal.valueOf(successCount).divide(new BigDecimal(sumCount)).setScale(2).doubleValue();
                    e.setSyncProgress(syncProgress);
                }
            });
        }

        return new TableResultResponse<ArtifactSyncRecordPageRes>(page.getTotal(), pageResult);
    }
}
