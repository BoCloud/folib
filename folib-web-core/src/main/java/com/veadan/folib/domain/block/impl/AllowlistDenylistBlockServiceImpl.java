package com.veadan.folib.domain.block.impl;

import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockQueryReq;
import com.veadan.folib.controllers.block.req.AllowlistDenylistBlockReq;
import com.veadan.folib.controllers.block.res.AllowlistDenylistBlockRes;
import com.veadan.folib.domain.block.AllowlistDenylistBlockService;
import com.veadan.folib.entity.License;
import com.veadan.folib.enums.BlockDomainEnum;
import com.veadan.folib.enums.CategoryEnum;
import com.veadan.folib.enums.RuleEnum;
import com.veadan.folib.entity.AllowlistDenylistBlock;
import com.veadan.folib.enums.TagEnum;
import com.veadan.folib.forms.configuration.SecurityPolicyConfigurationForm;
import com.veadan.folib.mapper.AllowlistDenylistBlockMapper;
import com.veadan.folib.mapper.LicenseMapper;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.services.SecurityPolicyConfigurationService;
import com.veadan.folib.storage.repository.RepositoryDto;
import org.springframework.beans.factory.annotation.Autowired;
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

    @Autowired
    private LicenseMapper licenseMapper;
    @Inject
    private AllowlistDenylistBlockMapper allowlistDenylistBlockMapper;
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Inject
    private SecurityPolicyConfigurationService securityPolicyConfigurationService;


    @Override
    public AllowlistDenylistBlockRes queryAllowlistDenylistBlock(AllowlistDenylistBlockReq allowlistDenylistBlockReq) {
        AllowlistDenylistBlock entity = toAllowlistDenylistBlock.apply(allowlistDenylistBlockReq);
        AllowlistDenylistBlock block = allowlistDenylistBlockMapper.queryAllowlistDenylistBlock(entity);
        return toAllowlistDenylistBlockRes.apply(block);
    }

    @Override
    public TableResultResponse<AllowlistDenylistBlockRes> paginQuery(AllowlistDenylistBlockQueryReq req) {
        AllowlistDenylistBlock entity = queryReqToAllowlistDenylistBlock.apply(req);
        long total = allowlistDenylistBlockMapper.count(entity);
        PageRequest pageRequest = PageRequest.of(req.getPage() - 1, req.getSize());
        List<AllowlistDenylistBlock> list = allowlistDenylistBlockMapper.queryAllByLimit(entity, pageRequest);
        return new TableResultResponse<>(total, list.stream().map(toAllowlistDenylistBlockRes).collect(Collectors.toList()));
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public AllowlistDenylistBlockRes insert(AllowlistDenylistBlockReq allowlistDenylistBlock) {
        AllowlistDenylistBlock entity = toAllowlistDenylistBlock.apply(allowlistDenylistBlock);
        AllowlistDenylistBlock block = allowlistDenylistBlockMapper.queryAllowlistDenylistBlock(new AllowlistDenylistBlock().setIdentifier(allowlistDenylistBlock.getIdentifier()).setDomain(allowlistDenylistBlock.getDomain()));
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


    @Override
    public List<AllowlistDenylistBlock> queryAllowlistDenylistBlockList(AllowlistDenylistBlock entity) {
        long total = allowlistDenylistBlockMapper.count(entity);
        total = total == 0 ? 1 : total;
        PageRequest pageRequest = PageRequest.of(0, (int) total);
        return allowlistDenylistBlockMapper.queryAllByLimit(entity, pageRequest);
    }

    @PostConstruct
    @Transactional(rollbackFor = Exception.class)
    public void initData() {
        AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder().tag(TagEnum.DEFAULT.toString()).build();
        long total = allowlistDenylistBlockMapper.count(entity);
        //初始化老数据
        if (total == 0) {
            //平台数据初始化
            this.initPlatformVulnerabilities();
            //仓库数据初始化
            this.initRepositoryVulnerabilities();
            //许可证数据初始化
            this.initLicenses();
        }
    }

    @Transactional(rollbackFor = Exception.class)
    public void initPlatformVulnerabilities() {
        SecurityPolicyConfigurationForm source = securityPolicyConfigurationService.config();
        List<AllowlistDenylistBlock> vulnerabilityList = new ArrayList<>();
        Date createdTime = new Date();
        for (String vulnerability : source.getWhites()) {
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(vulnerability)
                    .type(RuleEnum.WHITES.toString())
                    .category(CategoryEnum.VULNERABILITY.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .domain(BlockDomainEnum.PLATFORM.toString())
                    .createdTime(createdTime)
                    .build();
            vulnerabilityList.add(entity);
        }
        for (String vulnerability : source.getBlacks()) {
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(vulnerability)
                    .type(RuleEnum.BLACKLIST.toString())
                    .category(CategoryEnum.VULNERABILITY.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .domain(BlockDomainEnum.PLATFORM.toString())
                    .createdTime(createdTime)
                    .build();
            vulnerabilityList.add(entity);
        }
        if (!vulnerabilityList.isEmpty()) {
            allowlistDenylistBlockMapper.insertBatch(vulnerabilityList);
        }
    }
    @Transactional(rollbackFor = Exception.class)
    public void initRepositoryVulnerabilities() {
        MutableConfiguration configuration = configurationManagementService.getMutableConfigurationClone();
        Date createdTime = new Date();
        List<AllowlistDenylistBlock> list = new ArrayList<>();
        for (String strorageId : configuration.getStorages().keySet()) {
            for (String repositoryId : configuration.getStorages().get(strorageId).getRepositories().keySet()) {
                RepositoryDto dto = configuration.getStorages().get(strorageId).getRepository(repositoryId);
                String storageIdAndRepositoryId = dto.getStorageIdAndRepositoryId();
                if (!dto.getVulnerabilityBlacks().isEmpty()) {
                    for (String key : dto.getVulnerabilityBlacks()) {
                        AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                                .identifier(key)
                                .type(RuleEnum.BLACKLIST.toString())
                                .category(CategoryEnum.VULNERABILITY.toString())
                                .tag(TagEnum.DEFAULT.toString())
                                .domain(BlockDomainEnum.REPOSITORY.toString())
                                .correlationId(storageIdAndRepositoryId)
                                .createdTime(createdTime)
                                .build();
                        list.add(entity);
                    }
                }
                if (!dto.getVulnerabilityWhites().isEmpty()) {
                    for (String key : dto.getVulnerabilityWhites()) {
                        AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                                .identifier(key)
                                .type(RuleEnum.WHITES.toString())
                                .category(CategoryEnum.VULNERABILITY.toString())
                                .tag(TagEnum.DEFAULT.toString())
                                .domain(BlockDomainEnum.REPOSITORY.toString())
                                .correlationId(storageIdAndRepositoryId)
                                .createdTime(createdTime)
                                .build();
                        list.add(entity);
                    }
                }
            }
        }
        if (!list.isEmpty()) {
            allowlistDenylistBlockMapper.insertBatch(list);
        }
    }


    @Transactional(rollbackFor = Exception.class)
    public void initLicenses() {
        List<AllowlistDenylistBlock> licenseList = new ArrayList<>();
        List<License> whitesList = licenseMapper.selectLicense(null, null, 1, null, null);
        for (License license : whitesList) {
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(license.getLicenseId())
                    .type(RuleEnum.WHITES.toString())
                    .category(CategoryEnum.LICENSE.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .domain(BlockDomainEnum.PLATFORM.toString())
                    .createdTime(new Date())
                    .build();
            licenseList.add(entity);
        }
        List<License> blackList = licenseMapper.selectLicense(null, null, 2, null, null);
        for (License license : blackList) {
            AllowlistDenylistBlock entity = AllowlistDenylistBlock.builder()
                    .identifier(license.getLicenseId())
                    .type(RuleEnum.BLACKLIST.toString())
                    .category(CategoryEnum.LICENSE.toString())
                    .domain(BlockDomainEnum.PLATFORM.toString())
                    .tag(TagEnum.DEFAULT.toString())
                    .createdTime(new Date())
                    .build();
            licenseList.add(entity);
        }
        if (!licenseList.isEmpty()) {
            allowlistDenylistBlockMapper.insertBatch(licenseList);
        }
    }


    Function<AllowlistDenylistBlockReq, AllowlistDenylistBlock> toAllowlistDenylistBlock = (allowlistDenylistBlockReq) -> {
        if (allowlistDenylistBlockReq == null) {
            return null;
        }
        return AllowlistDenylistBlock.builder()
                .id(allowlistDenylistBlockReq.getId())
                .identifier(allowlistDenylistBlockReq.getIdentifier())
                .type(allowlistDenylistBlockReq.getType())
                .validFrom(allowlistDenylistBlockReq.getValidFrom())
                .tag(allowlistDenylistBlockReq.getTag())
                .domain(allowlistDenylistBlockReq.getDomain())
                .correlationId(allowlistDenylistBlockReq.getCorrelationId())
                .category(allowlistDenylistBlockReq.getCategory())
                .createdBy(allowlistDenylistBlockReq.getCreatedBy())
                .updatedBy(allowlistDenylistBlockReq.getUpdatedBy())
                .createdTime(allowlistDenylistBlockReq.getCreatedTime())
                .updateTime(allowlistDenylistBlockReq.getUpdateTime())
                .build();
    };

    Function<AllowlistDenylistBlock, AllowlistDenylistBlockRes> toAllowlistDenylistBlockRes = (allowlistDenylistBlock) -> {
        if (allowlistDenylistBlock == null) {
            return null;
        }
        return AllowlistDenylistBlockRes.builder()
                .id(allowlistDenylistBlock.getId())
                .identifier(allowlistDenylistBlock.getIdentifier())
                .type(allowlistDenylistBlock.getType())
                .validFrom(allowlistDenylistBlock.getValidFrom())
                .tag(allowlistDenylistBlock.getTag())
                .domain(allowlistDenylistBlock.getDomain())
                .correlationId(allowlistDenylistBlock.getCorrelationId())
                .category(allowlistDenylistBlock.getCategory())
                .createdBy(allowlistDenylistBlock.getCreatedBy())
                .updatedBy(allowlistDenylistBlock.getUpdatedBy())
                .createdTime(allowlistDenylistBlock.getCreatedTime())
                .updateTime(allowlistDenylistBlock.getUpdateTime())
                .build();
    };

    Function<AllowlistDenylistBlockQueryReq,AllowlistDenylistBlock> queryReqToAllowlistDenylistBlock = (allowlistDenylistBlockQueryReq) -> {
        if (allowlistDenylistBlockQueryReq == null) {
            return null;
        }
        AllowlistDenylistBlock allowlistDenylistBlock = new AllowlistDenylistBlock();
        allowlistDenylistBlock.setIdentifier(allowlistDenylistBlockQueryReq.getIdentifier());
        allowlistDenylistBlock.setType(allowlistDenylistBlockQueryReq.getType());
        allowlistDenylistBlock.setValidFrom(allowlistDenylistBlockQueryReq.getValidFrom());
        allowlistDenylistBlock.setTag(allowlistDenylistBlockQueryReq.getTag());
        allowlistDenylistBlock.setDomain(allowlistDenylistBlockQueryReq.getDomain());
        allowlistDenylistBlock.setCorrelationId(allowlistDenylistBlockQueryReq.getCorrelationId());
        allowlistDenylistBlock.setCategory(allowlistDenylistBlockQueryReq.getCategory());
        return allowlistDenylistBlock;
    };

}
