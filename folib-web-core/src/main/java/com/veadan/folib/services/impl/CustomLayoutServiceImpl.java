package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.veadan.folib.components.IdGenerateUtils;
import com.veadan.folib.domain.customlayout.CustomLayoutRecord;
import com.veadan.folib.entity.CustomLayout;
import com.veadan.folib.forms.customlayout.CustomLayoutForm;
import com.veadan.folib.mapper.CustomLayoutMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.CustomLayoutService;
import com.veadan.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 * @date 2024/12/26
 **/
@Slf4j
@Service
public class CustomLayoutServiceImpl implements CustomLayoutService {

    @Autowired
    private CustomLayoutMapper customLayoutMapper;

    @Autowired
    private IdGenerateUtils idGenerateUtils;

    @Override
    public TableResultResponse<CustomLayoutRecord> queryCustomLayoutPage(Integer page, Integer limit, CustomLayoutForm customLayoutForm) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Page<Object> result = PageHelper.startPage(page, limit);
        List<CustomLayout> customLayoutList = customLayoutMapper.selectList(Wrappers.<CustomLayout>lambdaQuery()
                .ge(CustomLayout::getId,0)
                .eq(StringUtils.isNotBlank(customLayoutForm.getLayoutName()), CustomLayout::getLayoutName, customLayoutForm.getLayoutName())
                .like(StringUtils.isNotBlank(customLayoutForm.getMatchLayoutName()), CustomLayout::getLayoutName, customLayoutForm.getMatchLayoutName())
        );
        return new TableResultResponse<CustomLayoutRecord>(result.getTotal(), Optional.ofNullable(customLayoutList).orElse(Collections.emptyList()).stream()
                .map(item -> {
                    CustomLayoutRecord customLayoutRecord = CustomLayoutRecord.builder().build();
                    BeanUtils.copyProperties(item, customLayoutRecord);
                    customLayoutRecord.setId(item.getId().toString());
                    return customLayoutRecord;
                }).collect(Collectors.toList()));
    }

    @Override
    public List<CustomLayoutRecord> queryCustomLayoutList(CustomLayoutForm customLayoutForm) {
        List<CustomLayout> customLayoutList = customLayoutMapper.selectList(Wrappers.<CustomLayout>lambdaQuery()
                .ge(CustomLayout::getId,0)
                .eq(StringUtils.isNotBlank(customLayoutForm.getLayoutName()), CustomLayout::getLayoutName, customLayoutForm.getLayoutName())
                .eq(StringUtils.isNotBlank(customLayoutForm.getMatchLayoutName()), CustomLayout::getLayoutName, customLayoutForm.getMatchLayoutName())
        );
        return Optional.ofNullable(customLayoutList).orElse(Collections.emptyList()).stream()
                .map(item -> {
                    CustomLayoutRecord customLayoutRecord = CustomLayoutRecord.builder().build();
                    BeanUtils.copyProperties(item, customLayoutRecord);
                    customLayoutRecord.setId(item.getId().toString());
                    return customLayoutRecord;
                }).collect(Collectors.toList());
    }

    @Override
    public CustomLayoutForm queryCustomLayout(CustomLayout customLayout) {
        CustomLayoutForm customLayoutForm = null;
        CustomLayout existsCustomLayout = getCustomLayout(customLayout);
        if (Objects.nonNull(existsCustomLayout)) {
            customLayoutForm = CustomLayoutForm.builder().build();
            BeanUtils.copyProperties(existsCustomLayout, customLayoutForm);
            customLayoutForm.setId(existsCustomLayout.getId().toString());
        }
        return customLayoutForm;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveCustomLayout(CustomLayoutForm customLayoutForm) {
        String username = UserUtils.getUsername();
        Date now = new Date();
        Long customLayoutId = idGenerateUtils.generateId("customLayoutId");
        CustomLayout customLayout = CustomLayout.builder().id(customLayoutId).layoutName(customLayoutForm.getLayoutName()).artifactPathPattern(customLayoutForm.getArtifactPathPattern()).createBy(username)
                .createTime(now).updateBy(username).updateTime(now).build();
        customLayoutMapper.insert(customLayout);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateCustomLayout(CustomLayoutForm customLayoutForm) {
        CustomLayout existsCustomLayout = getCustomLayout(CustomLayout.builder().layoutName(customLayoutForm.getLayoutName()).build());
        if (Objects.isNull(existsCustomLayout)) {
            return;
        }
        String username = UserUtils.getUsername();
        Date now = new Date();
        Long customLayoutId = existsCustomLayout.getId();
        CustomLayout customLayout = CustomLayout.builder().id(customLayoutId).artifactPathPattern(customLayoutForm.getArtifactPathPattern()).updateBy(username).updateTime(now).build();
        customLayoutMapper.updateById(customLayout);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteCustomLayout(CustomLayout customLayout) {
        CustomLayout existsCustomLayout = getCustomLayout(customLayout);
        if (Objects.isNull(existsCustomLayout)) {
            return;
        }
        customLayoutMapper.deleteById(existsCustomLayout.getId());
    }

    @Override
    public CustomLayout getCustomLayout(CustomLayout customLayout) {
        return customLayoutMapper.selectOne(Wrappers.<CustomLayout>lambdaQuery()
                .eq(customLayout.getLayoutName()!=null,CustomLayout::getLayoutName, customLayout.getLayoutName())
                .eq(customLayout.getId()!=null, CustomLayout::getId,customLayout.getId())
                .eq(customLayout.getArtifactPathPattern()!=null,CustomLayout::getArtifactPathPattern, customLayout.getArtifactPathPattern())
                .eq(customLayout.getCreateBy()!=null,CustomLayout::getCreateBy, customLayout.getCreateBy())
                .eq(customLayout.getCreateTime()!=null,CustomLayout::getCreateTime, customLayout.getCreateTime())
                .eq(customLayout.getUpdateBy()!=null,CustomLayout::getUpdateBy, customLayout.getUpdateBy())
                .eq(customLayout.getUpdateTime()!=null,CustomLayout::getUpdateTime, customLayout.getUpdateTime())

        );
    }
}
