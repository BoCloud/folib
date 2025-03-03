package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollUtil;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.veadan.folib.constant.ArtifactSyncRecordStatusEnum;
import com.veadan.folib.dto.ArtifactSyncRecordCountDto;
import com.veadan.folib.dto.FileSizeStatisticsDto;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.ArtifactSyncSlaveRecord;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.mapper.ArtifactSyncSlaveRecordMapper;
import com.veadan.folib.model.request.ArtifactSyncRecordPageReq;
import com.veadan.folib.model.response.ArtifactSyncRecordCountRes;
import com.veadan.folib.model.response.ArtifactSyncRecordPageRes;
import com.veadan.folib.model.response.FileSizeStatisticsRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ArtifactSyncRecordService;
import io.micrometer.core.instrument.util.StringUtils;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.function.Function;
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

    private static final BigDecimal KILOBYTE = new BigDecimal(1024);
    private static final BigDecimal MEGABYTE = KILOBYTE.multiply(KILOBYTE);
    private static final BigDecimal GIGABYTE = MEGABYTE.multiply(KILOBYTE);
    @Override
    public TableResultResponse<ArtifactSyncRecordPageRes> page(ArtifactSyncRecordPageReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final Integer pageNumber = model.getPageNumber();
        final Integer pageSize = model.getPageSize();
        final Page<Object> page = PageHelper.startPage(pageNumber, pageSize);
        final Example recordExample = Example.builder(ArtifactSyncRecord.class).build();
        recordExample.setOrderByClause("create_time desc");
        if (StringUtils.isNotEmpty(storageId)) {
            recordExample.and().andEqualTo("sourceStorageId", storageId);
        }
        if (StringUtils.isNotEmpty(repositoryId)) {
            recordExample.and().andEqualTo("sourceRepositoryId", repositoryId);
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
                e.setSlaveRecordCleared(e.getSyncProgress() != null);
                if (!e.getSlaveRecordCleared()) {
                    final Map<Integer, Long> groupStatusMap = groupSyncNoSlaveRecordCountMap.getOrDefault(e.getSyncNo(), Collections.emptyMap());
                    final int sumCount = groupStatusMap.values().stream().mapToInt(Long::intValue).sum();
                    final Long successCount = groupStatusMap.getOrDefault(ArtifactSyncRecordStatusEnum.SUCCESS.getVal(), 0L);
                    e.setSyncProgress(0D);
                    if (successCount > 0) {
                        final double syncProgress = BigDecimal.valueOf(successCount).divide(new BigDecimal(sumCount), 2, RoundingMode.HALF_UP).setScale(2).doubleValue();
                        e.setSyncProgress(syncProgress);
                        //if (syncProgress >= 1.0D) {
                        //    e.setStatus(ArtifactSyncRecordStatusEnum.SUCCESS.getVal());
                        //}
                    }
                }
            });
        }

        return new TableResultResponse<ArtifactSyncRecordPageRes>(page.getTotal(), pageResult);
    }

    /**
     * 统计分发晋级 days 天数内 的数量
     *
     * @param days 天数
     * @return ArtifactSyncRecordCountRes
     */
    @Override
    public ArtifactSyncRecordCountRes getCount(Integer days) {
        //ArtifactSyncRecordCountDto artifactSyncRecordCountDto = artifactSyncRecordMapper.countArtifactSyncRecord(days);
        ArtifactSyncRecordCountRes countRes = new ArtifactSyncRecordCountRes();

        //if (artifactSyncRecordCountDto == null) {
        //     return countRes.setFailedCount(0L).setSuccessCount(0L).setTotalCount(0L).setFileSizeCount(new BigDecimal("0")).setDate(null);
        //}

        Long successCount = artifactSyncRecordMapper.statCount(days, ArtifactSyncRecordStatusEnum.SUCCESS.getVal());
        Long failCount = artifactSyncRecordMapper.statCount(days, ArtifactSyncRecordStatusEnum.FAILED.getVal());
        Long totalCount = artifactSyncRecordMapper.statCount(days, null);
        if(totalCount==null || totalCount==0){
            return countRes.setFailedCount(0L).setSuccessCount(0L).setTotalCount(0L).setFileSizeCount(new BigDecimal("0")).setDate(null);
        }
        countRes.setFailedCount(failCount);
        countRes.setSuccessCount(successCount);
        countRes.setTotalCount(totalCount);
        Long fileSizeCount = artifactSyncSlaveRecordMapper.statisticsFileSize(days);
        BigDecimal fileSizeInGB =   convertBytesToGB(fileSizeCount);
        countRes.setFileSizeCount(fileSizeInGB.setScale(4, RoundingMode.HALF_UP));
        return countRes;
    }

    Function<ArtifactSyncRecordCountDto,ArtifactSyncRecordCountRes>  dtoToRes= (dto) -> {
        return new ArtifactSyncRecordCountRes()
                .setSuccessCount(dto.getSuccessCount())
                .setFailedCount(dto.getFailedCount())
                .setTotalCount(dto.getTotalCount())
                .setDate(dto.getDate());
    };
    /**
     * 统计分发晋级 days 天数内 成功失败的数量
     *
     * @param days 天数
     * @return ArtifactSyncRecordCountRes
     */
    @Override
    public List<ArtifactSyncRecordCountRes> getStatusTrends(Integer days) {
       List<ArtifactSyncRecordCountDto>  dtoList= artifactSyncRecordMapper.countByDateArtifactSyncRecord(days);
       if (CollectionUtils.isEmpty(dtoList)){
           return List.of();
       }
        return dtoList.stream().map(dto -> dtoToRes.apply(dto)).collect(Collectors.toList());
    }

    public  BigDecimal convertBytesToGB(Long fileSizeInBytes) {
        if (Objects.isNull(fileSizeInBytes)) {
            return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        }
        BigDecimal fileSize = new BigDecimal(fileSizeInBytes);
        return fileSize.divide(GIGABYTE, 2, RoundingMode.HALF_UP);
    }

    /**
     * 统计某些天内晋级或分发文件（大小） 排名靠前的仓库
     *
     * @param days        天数
     * @param limitNumber 统计数量
     * @return FileSizeStatisticsRes
     */
    @Override
    public List<FileSizeStatisticsRes> fileSizeStatisticsByWarehouse(Integer days, Integer limitNumber) {
        List<FileSizeStatisticsDto>  dtoList = artifactSyncRecordMapper.fileSizeStatisticsByWarehouse(days,limitNumber);
        if(CollectionUtils.isEmpty(dtoList)){
            return List.of();
        }
       return dtoList.stream().map(item->{
            return new FileSizeStatisticsRes()
                    .setFileSize(convertBytesToGB(item.getFileSize()))
                    .setRepositoryId(item.getRepositoryId());
        }).collect(Collectors.toList());
    }
}
