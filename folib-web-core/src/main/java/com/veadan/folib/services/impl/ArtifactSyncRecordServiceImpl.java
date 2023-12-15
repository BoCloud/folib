package com.veadan.folib.services.impl;

import cn.hutool.core.collection.CollUtil;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.veadan.folib.entity.ArtifactSyncRecord;
import com.veadan.folib.entity.ExternalNode;
import com.veadan.folib.mapper.ArtifactSyncRecordMapper;
import com.veadan.folib.model.request.ArtifactSyncRecordPageReq;
import com.veadan.folib.model.response.ArtifactSyncRecordPageRes;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ArtifactSyncRecordService;
import io.micrometer.core.instrument.util.StringUtils;
import org.springframework.stereotype.Service;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;
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
    
    
    @Override
    public TableResultResponse<ArtifactSyncRecordPageRes> page(ArtifactSyncRecordPageReq model) {
        final String storageId = model.getStorageId();
        final String repositoryId = model.getRepositoryId();
        final Integer pageNumber = model.getPageNumber();
        final Integer pageSize = model.getPageSize();
        final Page<Object> page = PageHelper.startPage(pageNumber, pageSize);
        final Example example = Example.builder(ArtifactSyncRecord.class).build();
        example.setOrderByClause("create_time desc");
        if (StringUtils.isNotEmpty(storageId) && StringUtils.isNotEmpty(repositoryId)) {
            example.and().andLike("sourcePath", storageId + "/" + repositoryId + "/%");
        }
        
        final List<ArtifactSyncRecordPageRes> pageResult = Optional.ofNullable(artifactSyncRecordMapper.selectByExample(example))
                .filter(CollUtil::isNotEmpty)
                .orElse(Collections.emptyList())
                .stream().map(ArtifactSyncRecordPageRes::new)
                .collect(Collectors.toList());

        return new TableResultResponse<ArtifactSyncRecordPageRes>(page.getTotal(), pageResult);
    }
}
