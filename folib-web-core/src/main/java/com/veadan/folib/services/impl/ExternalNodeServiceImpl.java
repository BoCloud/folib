package com.veadan.folib.services.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import com.google.common.collect.Lists;
import com.veadan.folib.entity.ExternalNode;
import com.veadan.folib.enums.ArtifactoryFolibRepositoryTypeEnum;
import com.veadan.folib.forms.externalnode.ExternalNodeForm;
import com.veadan.folib.forms.externalnode.ExternalNodeRepositoryForm;
import com.veadan.folib.forms.externalnode.RepositoryForm;
import com.veadan.folib.mapper.ExternalNodeMapper;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ExternalNodeService;
import com.veadan.folib.services.JFrogService;
import com.veadan.folib.util.RSAUtils;
import com.veadan.folib.ws.server.CheckTargetNodeRepositoryCommandProcessor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jfrog.artifactory.client.model.LightweightRepository;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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

    @Lazy
    @Inject
    private JFrogService jFrogService;

    @Inject
    private RSAUtils rsaUtils;
    @Autowired
    private CheckTargetNodeRepositoryCommandProcessor checkTargetNodeRepositoryCommandProcessor;

    @Override
    public TableResultResponse<ExternalNodeForm> queryExternalNodeList(Integer page, Integer limit, ExternalNodeForm externalNodeForm) {
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 10;
        }
        String nodeName = "";
        if (StringUtils.isNotBlank(externalNodeForm.getNodeName())) {
             nodeName = "%" + externalNodeForm.getNodeName() + "%";
        }

        Page<Object> result = PageHelper.startPage(page, limit);
        List<ExternalNode> externalNodeList = externalNodeMapper.selectList(Wrappers.<ExternalNode>lambdaQuery()
                .like(ExternalNode::getNodeName,nodeName)
                .orderByAsc(ExternalNode::getCreateTime)
        );
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
            //externalNodeMapper.deleteByPrimaryKey(id);
            externalNodeMapper.selectById(id);
        }
    }

    @Override
    public List<ExternalNodeRepositoryForm> getExternalNodeRepositories(String type) {
        if (StringUtils.isBlank(type)) {
            type = ArtifactoryFolibRepositoryTypeEnum.GENERIC.getFoLibraryName();
        }
        List<String> packageTypes = Lists.newArrayList(ArtifactoryFolibRepositoryTypeEnum.queryNameByFoLibraryName(type));
        List<ExternalNode> externalNodeList = externalNodeMapper.selectList(Wrappers.<ExternalNode>lambdaQuery().orderByAsc(ExternalNode::getCreateTime));
        return Optional.ofNullable(externalNodeList).orElse(Collections.emptyList()).stream().map(externalNode -> {
            ExternalNodeRepositoryForm externalNodeRepositoryForm = ExternalNodeRepositoryForm.builder().build();
            BeanUtils.copyProperties(externalNode, externalNodeRepositoryForm);
            externalNodeRepositoryForm.setKey(externalNodeRepositoryForm.getNodeName());
            List<LightweightRepository> lightweightRepositories = jFrogService.listRepository(externalNode.getAddress(), externalNode.getUsername(), rsaUtils.decrypt(externalNode.getPassword()), packageTypes);
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
            externalNodeMapper.insert(externalNode);
        } else {
            ExternalNode existsExternalNode = selectExternalNode(externalNodeForm);
            if (Objects.nonNull(existsExternalNode)) {
                if (StringUtils.isBlank(externalNode.getPassword())) {
                    externalNode.setPassword(null);
                }
                externalNodeMapper.update(externalNode,Wrappers.<ExternalNode>lambdaUpdate()
                        .eq(ExternalNode::getId, existsExternalNode.getId())
                );
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
            externalNode = externalNodeMapper.selectOne(Wrappers.<ExternalNode>lambdaQuery().eq(ExternalNode::getId, externalNodeForm.getId()));
        } else if (StringUtils.isNotBlank(externalNodeForm.getNodeName())) {
            List<ExternalNode> externalNodeList = externalNodeMapper.selectList(Wrappers.<ExternalNode>lambdaQuery().eq(ExternalNode::getNodeName, externalNodeForm.getNodeName()));
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
       long count =  externalNodeMapper.selectCount(Wrappers.<ExternalNode>lambdaQuery()
                .eq(ExternalNode::getNodeName, nodeName)
                .ne(Objects.nonNull(id),ExternalNode::getId, id)
        );
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
