package com.veadan.folib.services.impl;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.veadan.folib.entity.ExternalNode;
import com.veadan.folib.enums.ArtifactoryFolibRepositoryTypeEnum;
import com.veadan.folib.enums.ArtifactoryRepositoryTypeEnum;
import com.veadan.folib.forms.externalnode.ExternalNodeForm;
import com.veadan.folib.forms.externalnode.ExternalNodeRepositoryForm;
import com.veadan.folib.forms.externalnode.RepositoryForm;
import com.veadan.folib.mapper.ExternalNodeMapper;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ExternalNodeService;
import com.veadan.folib.services.JFrogService;
import com.veadan.folib.util.RSAUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jfrog.artifactory.client.model.LightweightRepository;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import tk.mybatis.mapper.entity.Example;

import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class ExternalNodeServiceImpl implements ExternalNodeService {

    @Inject
    private ExternalNodeMapper externalNodeMapper;

    @Inject
    private JFrogService jFrogService;

    @Inject
    private RSAUtils rsaUtils;

    @Override
    public TableResultResponse<ExternalNodeForm> queryExternalNodeList(Integer page, Integer limit, ExternalNodeForm externalNodeForm) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        Example example = Example.builder(ExternalNode.class).build();
        Example.Criteria criteria = example.createCriteria();
        if (StringUtils.isNotBlank(externalNodeForm.getNodeName())) {
            String nodeName = "%" + externalNodeForm.getNodeName() + "%";
            criteria.andLike("nodeName", nodeName);
        }
        example.setOrderByClause("create_time");
        Page<Object> result = PageHelper.startPage(page, limit);
        List<ExternalNode> externalNodeList = externalNodeMapper.selectByExample(example);
        return new TableResultResponse<ExternalNodeForm>(result.getTotal(), Optional.ofNullable(externalNodeList).orElse(Collections.emptyList()).stream().map(externalNode -> {
            ExternalNodeForm resultExternalNode = ExternalNodeForm.builder().build();
            BeanUtils.copyProperties(externalNode, resultExternalNode);
            resultExternalNode.setPassword("");
            return resultExternalNode;
        }).collect(Collectors.toList()));
    }

    @Override
    public ExternalNodeForm getExternalNode(ExternalNodeForm externalNodeForm) {
        ExternalNodeForm resultExternalNode = null;
        ExternalNode externalNode = selectExternalNode(externalNodeForm);
        if (Objects.nonNull(externalNode)) {
            resultExternalNode = ExternalNodeForm.builder().build();
            BeanUtils.copyProperties(externalNode, resultExternalNode);
        }
        return resultExternalNode;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveExternalNode(ExternalNodeForm externalNodeForm) {
        saveOrUpdateExternalNode(externalNodeForm);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateExternalNode(ExternalNodeForm externalNodeForm) {
        saveOrUpdateExternalNode(externalNodeForm);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteExternalNode(Long id) {
        ExternalNode externalNode = selectExternalNode(ExternalNodeForm.builder().id(id).build());
        if (Objects.nonNull(externalNode)) {
            externalNodeMapper.deleteByPrimaryKey(id);
        }
    }

    @Override
    public List<ExternalNodeRepositoryForm> getExternalNodeRepositories() {
        Example example = Example.builder(ExternalNode.class).build();
        Example.Criteria criteria = example.createCriteria();
        example.setOrderByClause("create_time");
        List<ExternalNode> externalNodeList = externalNodeMapper.selectByExample(example);
        return Optional.ofNullable(externalNodeList).orElse(Collections.emptyList()).stream().map(externalNode -> {
            ExternalNodeRepositoryForm externalNodeRepositoryForm = ExternalNodeRepositoryForm.builder().build();
            BeanUtils.copyProperties(externalNode, externalNodeRepositoryForm);
            externalNodeRepositoryForm.setKey(externalNodeRepositoryForm.getNodeName());
            List<LightweightRepository> lightweightRepositories = jFrogService.listRepository(externalNode.getAddress(), externalNode.getUsername(), rsaUtils.decrypt(externalNode.getPassword()), Lists.newArrayList(ArtifactoryFolibRepositoryTypeEnum.GENERIC.getName(), ArtifactoryFolibRepositoryTypeEnum.MAVEN.getName(), ArtifactoryFolibRepositoryTypeEnum.DOCKER.getName()));
            externalNodeRepositoryForm.setRepositories(Optional.ofNullable(lightweightRepositories).orElse(Collections.emptyList()).stream().map(lightweightRepository -> RepositoryForm.builder().name(lightweightRepository.getKey()).key(String.format("%s,%s", externalNodeRepositoryForm.getKey(), lightweightRepository.getKey())).artifactoryRepositoryType(externalNodeRepositoryForm.getType()).build()).collect(Collectors.toList()));
            return externalNodeRepositoryForm;
        }).collect(Collectors.toList());
    }

    /**
     * 保存或修改外部节点
     *
     * @param externalNodeForm 表单参数
     */
    private void saveOrUpdateExternalNode(ExternalNodeForm externalNodeForm) {
        ExternalNode externalNode = ExternalNode.builder().build();
        BeanUtils.copyProperties(externalNodeForm, externalNode);
        checkNodeName(externalNode.getId(), externalNode.getNodeName());
        validateArtifactory(externalNode);
        if (Objects.isNull(externalNode.getId())) {
            externalNode.setCreateTime(new Date());
            externalNodeMapper.insertSelective(externalNode);
        } else {
            ExternalNode existsExternalNode = selectExternalNode(externalNodeForm);
            if (Objects.nonNull(existsExternalNode)) {
                if (StringUtils.isBlank(externalNode.getPassword())) {
                    externalNode.setPassword(null);
                }
                externalNodeMapper.updateByPrimaryKeySelective(externalNode);
            }
        }
    }

    /**
     * 获取外部节点
     *
     * @param externalNodeForm 表单参数
     * @return 外部节点
     */
    private ExternalNode selectExternalNode(ExternalNodeForm externalNodeForm) {
        ExternalNode externalNode = null;
        if (Objects.nonNull(externalNodeForm.getId())) {
            externalNode = externalNodeMapper.selectByPrimaryKey(externalNodeForm.getId());
        } else if (StringUtils.isNotBlank(externalNodeForm.getNodeName())) {
            Example example = Example.builder(ExternalNode.class).build();
            Example.Criteria criteria = example.createCriteria();
            criteria.andEqualTo("nodeName", externalNodeForm.getNodeName());
            List<ExternalNode> externalNodeList = externalNodeMapper.selectByExample(example);
            if (CollectionUtils.isNotEmpty(externalNodeList)) {
                externalNode = externalNodeList.get(0);
            }
        }
        return externalNode;
    }

    /**
     * 校验节点名称
     *
     * @param id       id
     * @param nodeName 节点名称
     */
    private void checkNodeName(Long id, String nodeName) {
        Example example = Example.builder(ExternalNode.class).build();
        Example.Criteria criteria = example.createCriteria();
        criteria.andEqualTo("nodeName", nodeName);
        if (Objects.nonNull(id)) {
            criteria.andNotEqualTo("id", id);
        }
        int count = externalNodeMapper.selectCountByExample(example);
        if (count > 0) {
            throw new BusinessException(String.format("节点%s已存在！！！", nodeName));
        }
    }

    /**
     * 校验外部节点
     *
     * @param externalNode 外部节点信息
     */
    private void validateArtifactory(ExternalNode externalNode) {
        String nodeName = externalNode.getNodeName();
        if (StringUtils.isBlank(externalNode.getPassword())) {
            externalNode = selectExternalNode(ExternalNodeForm.builder().nodeName(externalNode.getNodeName()).build());
        }
        if (Objects.isNull(externalNode)) {
            throw new BusinessException(String.format("未找到外部节点%s信息", nodeName));
        }
        String password = rsaUtils.decrypt(externalNode.getPassword());
        if (StringUtils.isBlank(password)) {
            throw new BusinessException("外部节点密码解析错误，请检查密码");
        }
        jFrogService.validateArtifactory(externalNode.getAddress(), externalNode.getUsername(), password);
    }
}
