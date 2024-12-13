package com.veadan.folib.domain.block.impl;

import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockQueryReq;
import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockReq;
import com.veadan.folib.controllers.block.res.AllowlistDenylistBlockRes;
import com.veadan.folib.domain.block.AllowlistDenylistBlockService;
import com.veadan.folib.entity.License;
import com.veadan.folib.enums.CategoryEnum;
import com.veadan.folib.enums.RuleEnum;
import com.veadan.folib.entity.AllowlistDenylistBlock;
import com.veadan.folib.enums.TagEnum;
import com.veadan.folib.forms.configuration.SecurityPolicyConfigurationForm;
import com.veadan.folib.mapper.AllowlistDenylistBlockMapper;
import com.veadan.folib.mapper.LicenseMapper;
import com.veadan.folib.services.SecurityPolicyConfigurationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
public class AllowlistDenylistBlockServiceImpl implements AllowlistDenylistBlockService {

    @Inject
    private AllowlistDenylistBlockMapper allowlistDenylistBlockMapper;

    @Inject
    private SecurityPolicyConfigurationService securityPolicyConfigurationService;

    @Autowired
    private LicenseMapper licenseMapper;


    @Override
    public AllowlistDenylistBlockRes queryAllowlistDenylistBlock(AllowlistDenylistBlockReq allowlistDenylistBlockReq) {
        AllowlistDenylistBlock entity = toAllowlistDenylistBlock.apply(allowlistDenylistBlockReq);
        AllowlistDenylistBlock block = allowlistDenylistBlockMapper.queryAllowlistDenylistBlock(entity);
        return toAllowlistDenylistBlockRes.apply(block);
    }

    @Override
    public Page<AllowlistDenylistBlockRes> paginQuery(AllowlistDenylistBlockQueryReq req) {
        AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder().identifier(req.getIdentifier()).type(req.getType()).category(req.getCategory()).build();
        long total = allowlistDenylistBlockMapper.count(entity);
        PageRequest pageRequest = PageRequest.of(req.getPage() - 1, req.getSize());
        List<AllowlistDenylistBlock> list = allowlistDenylistBlockMapper.queryAllByLimit(entity, pageRequest);
        return new PageImpl<>(list.stream().map(toAllowlistDenylistBlockRes).collect(Collectors.toList()), pageRequest, total);

    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public AllowlistDenylistBlockRes insert(AllowlistDenylistBlockReq allowlistDenylistBlock) {
        AllowlistDenylistBlock entity = toAllowlistDenylistBlock.apply(allowlistDenylistBlock);
        AllowlistDenylistBlock block = allowlistDenylistBlockMapper.queryAllowlistDenylistBlock(new AllowlistDenylistBlock().setIdentifier(allowlistDenylistBlock.getIdentifier()));
        if (block == null) {
            allowlistDenylistBlockMapper.insert(entity);
        } else if (RuleEnum.WHITES.toString().equals(allowlistDenylistBlock.getType()) && block.getType().equals(RuleEnum.WHITES.toString())) {
            throw new RuntimeException("不允许添加重复白名单");
        } else if (RuleEnum.BLACKLIST.toString().equals(allowlistDenylistBlock.getType()) && block.getType().equals(RuleEnum.BLACKLIST.toString())) {
            throw new RuntimeException("不允许添加重复黑名单");
        } else if (block.getType().equals(RuleEnum.BLACKLIST.toString()) && RuleEnum.WHITES.toString().equals(allowlistDenylistBlock.getType())) {
            throw new RuntimeException("黑名单中已经存在:" + allowlistDenylistBlock.getIdentifier());
        } else if (block.getType().equals(RuleEnum.WHITES.toString()) && RuleEnum.BLACKLIST.toString().equals(allowlistDenylistBlock.getType())) {
            throw new RuntimeException("白名单中已经存在:" + allowlistDenylistBlock.getIdentifier());
        }
        return toAllowlistDenylistBlockRes.apply(entity);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public AllowlistDenylistBlockRes update(AllowlistDenylistBlockReq allowlistDenylistBlock) {
        AllowlistDenylistBlock entity = toAllowlistDenylistBlock.apply(allowlistDenylistBlock);
        allowlistDenylistBlockMapper.update(entity);
        return toAllowlistDenylistBlockRes.apply(entity);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteAllowlistDenylistBlock(AllowlistDenylistBlockReq req) {
        AllowlistDenylistBlock entity = toAllowlistDenylistBlock.apply(req);
        int count = allowlistDenylistBlockMapper.deleteAllowlistDenylistBloc(entity);
        return count == 1;
    }
    @PostConstruct
    public void initData() {
        AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder().tag(TagEnum.DEFAULT.toString()).build();
        long total = allowlistDenylistBlockMapper.count(entity);
        if (total == 0) {
            //初始化老数据
            this.initVulnerabilities();
            this.initLicenses();
        }
    }
    @Transactional(rollbackFor = Exception.class)
    public void initVulnerabilities(){
        SecurityPolicyConfigurationForm source = securityPolicyConfigurationService.config();
        List<AllowlistDenylistBlock> vulnerabilityList = new ArrayList<>();
        for (String vulnerability : source.getWhites()){
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(vulnerability)
                    .type(RuleEnum.WHITES.toString())
                    .category(CategoryEnum.VULNERABILITY.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .createdTime(new Date())
                    .build();
            vulnerabilityList.add(entity);
        }
        for (String vulnerability : source.getBlacks()){
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(vulnerability)
                    .type(RuleEnum.BLACKLIST.toString())
                    .category(CategoryEnum.VULNERABILITY.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .createdTime(new Date())
                    .build();
            vulnerabilityList.add(entity);
        }
        if(!vulnerabilityList.isEmpty()){
            allowlistDenylistBlockMapper.insertBatch(vulnerabilityList);
        }
    }
    @Transactional(rollbackFor = Exception.class)
   public void initLicenses(){
        List<AllowlistDenylistBlock> licenseList = new ArrayList<>();
       List<License> whitesList = licenseMapper.selectLicense(null, null, 1, null, null);
        for (License license : whitesList){
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(license.getLicenseId())
                    .type(RuleEnum.WHITES.toString())
                    .category(CategoryEnum.LICENSE.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .createdTime(new Date())
                    .build();
            licenseList.add(entity);
        }
        List<License> blackList = licenseMapper.selectLicense(null, null, 2, null, null);
        for (License license : blackList){
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(license.getLicenseId())
                    .type(RuleEnum.BLACKLIST.toString())
                    .category(CategoryEnum.LICENSE.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .createdTime(new Date())
                    .build();
            licenseList.add(entity);
        }
        if(!licenseList.isEmpty()){
            allowlistDenylistBlockMapper.insertBatch(licenseList);
        }
    }


    Function<AllowlistDenylistBlockReq, AllowlistDenylistBlock> toAllowlistDenylistBlock = (allowlistDenylistBlockReq)-> {
        if(allowlistDenylistBlockReq==null){
            return null;
        }
        return   AllowlistDenylistBlock.builder()
                .id(allowlistDenylistBlockReq.getId())
                .identifier(allowlistDenylistBlockReq.getIdentifier())
                .type(allowlistDenylistBlockReq.getType())
                .validFrom(allowlistDenylistBlockReq.getValidFrom())
                .category(allowlistDenylistBlockReq.getCategory())
                .createdBy(allowlistDenylistBlockReq.getCreatedBy())
                .updatedBy(allowlistDenylistBlockReq.getUpdatedBy())
                .createdTime(allowlistDenylistBlockReq.getCreatedTime())
                .updateTime(allowlistDenylistBlockReq.getUpdateTime())
                .build();
    };

    Function<AllowlistDenylistBlock, AllowlistDenylistBlockRes> toAllowlistDenylistBlockRes = (allowlistDenylistBlock)-> {
        if(allowlistDenylistBlock==null){
            return null;
        }
        return AllowlistDenylistBlockRes.builder()
                .id(allowlistDenylistBlock.getId())
                .identifier(allowlistDenylistBlock.getIdentifier())
                .type(allowlistDenylistBlock.getType())
                .validFrom(allowlistDenylistBlock.getValidFrom())
                .category(allowlistDenylistBlock.getCategory())
                .createdBy(allowlistDenylistBlock.getCreatedBy())
                .updatedBy(allowlistDenylistBlock.getUpdatedBy())
                .createdTime(allowlistDenylistBlock.getCreatedTime())
                .updateTime(allowlistDenylistBlock.getUpdateTime())
                .build();
    };

}
