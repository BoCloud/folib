package com.veadan.folib.controllers.configuration;


import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.controllers.cluster.dto.SyncClusterDispatchDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.forms.configuration.ClusterDispatchNodeForm;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.DistributionService;
import com.veadan.folib.ws.server.PromotionTaskQueue;
import io.swagger.annotations.*;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;

import javax.websocket.Session;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Map;


/**
 * 制品分发集群配置逻辑控制层
 *
 * @author qijianping
 */
@RestController
@RequestMapping("/api/configuration/folib/dispatch")
@Api(description = "配置分发", tags = "配置分发")
public class ClusterDispatchConfigurationController extends BaseConfigurationController {

    @Autowired
    private ClusterDispatchManagementService clusterDispatchManagementService;

    @Autowired
    private ConversionService conversionService;

    @Autowired
    private ClusterSyncService clusterSyncService;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    @Autowired
    private PromotionTaskQueue promotionTaskQueue;
    @Autowired
    private DistributionService distributionService;


    protected ClusterDispatchConfigurationController(ConfigurationManagementService configurationManagementService) {
        super(configurationManagementService);
    }

    // 查询
    @ApiOperation(value = "query cluster dispatch config")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The dispatch config was queryed successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity queryClusterDispatch() {
        Map<String, ClusterDispatchNodeDto> map = configurationManagementService.
                getMutableConfigurationClone().getClusterDispatchNode();
        final Collection<ClusterDispatchNodeDto> values = map.values();
        values.forEach(nodeDto -> {
            String targetHostName = FolibWsRunManageUtil.getSimpleTargetHostName(nodeDto);
            Session session = folibWsRunManageV2.getSession(targetHostName);
            nodeDto.setWsClientOnline(session != null && session.isOpen());
        });


        return ResponseEntity.ok(values);
    }

    // 新增
    @ApiOperation(value = "Adds a cluster dispatch config")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The dispatch config was created successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity createClusterNode(@RequestBody ClusterDispatchNodeForm clusterDispatchNodeForm,
                                            BindingResult bindingResult,
                                            @RequestHeader(HttpHeaders.ACCEPT)
                                            String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("参数异常", bindingResult);
        }

            ClusterDispatchNodeDto existingNode;
            ClusterDispatchNodeDto nodeDto;
            synchronized (this) {
                Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
                existingNode = clusterDispatchNode.get(clusterDispatchNodeForm.getClusterEnName());

                // 创建分发节点
                nodeDto = new ClusterDispatchNodeDto();
                BeanUtils.copyProperties(clusterDispatchNodeForm, nodeDto);

                nodeDto.setCreateTime(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

                if (existingNode == null) {//新增操作
                    nodeDto.setAutoRegister(false);
                    String newHostName = FolibWsRunManageUtil.getSimpleTargetHostName(nodeDto);
                    ClusterDispatchNodeDto exitedHostNameNodeDto = clusterDispatchNode.values().stream().filter(dto -> {
                        String exitedHostName = FolibWsRunManageUtil.getSimpleTargetHostName(dto);
                        return exitedHostName.equals(newHostName);
                    }).findAny().orElse(null);
                    //已经存在这个 hostName了
                    if (exitedHostNameNodeDto != null) {
                        throw new RuntimeException(String.format("已经存在 %s 节点",newHostName));
                    }
                }
            }
        try {
            clusterDispatchManagementService.createClusterNode(nodeDto);

            // 向其他集群节点同步同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
            if (nodeDto.getAutoRegister() != null && nodeDto.getAutoRegister()) {
                return getSuccessfulResponseEntity("ok", accept);
            }
            handleWsServer(nodeDto);
            //重新注册晋级任务队列
            //reRegisterPromotionTaskQueue(existingNode, nodeDto);
            return getSuccessfulResponseEntity("ok", accept);
        } catch (BusinessException e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, e.getMessage(), e, accept);
        } catch (Exception e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "新增制品分发节点信息失败", e, accept);
        }
    }

    //更新
    @ApiOperation(value = "Updates a cluster dispatch config.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "The dispatch config was updated successfully."),
            @ApiResponse(code = 500, message = "An error occurred.")})
    @PreAuthorize("hasAuthority('CONFIGURATION_ADD_UPDATE_STORAGE')")
    @PutMapping(value = "/{clusterEnName}",
            consumes = MediaType.APPLICATION_JSON_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity updateClusterDispatch(
            @ApiParam(value = "The clusterEnName", required = true)
            @PathVariable String clusterEnName,
            @RequestBody ClusterDispatchNodeForm clusterDispatchNodeForm,
            BindingResult bindingResult,
            @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        if (bindingResult.hasErrors()) {
            throw new RequestBodyValidationException("参数异常", bindingResult);
        }


            ClusterDispatchNodeDto existingNode;
            ClusterDispatchNodeDto nodeDto;
            synchronized (this) {
                Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
                existingNode = clusterDispatchNode.get(clusterDispatchNodeForm.getClusterEnName());

                // 创建分发节点
                nodeDto = new ClusterDispatchNodeDto();
                BeanUtils.copyProperties(clusterDispatchNodeForm, nodeDto);
                nodeDto.setCreateTime(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

                if (existingNode == null) {//新增操作
                    nodeDto.setAutoRegister(false);
                    String newHostName = FolibWsRunManageUtil.getSimpleTargetHostName(nodeDto);
                    ClusterDispatchNodeDto exitedHostNameNodeDto = clusterDispatchNode.values().stream().filter(dto -> {
                        String exitedHostName = FolibWsRunManageUtil.getSimpleTargetHostName(dto);
                        return exitedHostName.equals(newHostName);
                    }).findAny().orElse(null);
                    //已经存在这个 hostName了
                    if (exitedHostNameNodeDto != null) {
                        throw new RuntimeException(String.format("已经存在 %s 节点",newHostName));
                    }
                }
            }
        try {
            clusterDispatchManagementService.createClusterNode(nodeDto);

            // 向其他集群节点同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
            if (nodeDto.getAutoRegister() != null && nodeDto.getAutoRegister()) {
                return getSuccessfulResponseEntity("ok", accept);
            }
            handleWsServer(nodeDto);
            //重新注册晋级任务队列
            //reRegisterPromotionTaskQueue(existingNode, nodeDto);
            return getSuccessfulResponseEntity("ok", accept);
        } catch (Exception e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "修改制品分发节点信息失败", e, accept);
        }
    }

    //public synchronized void reRegisterPromotionTaskQueue(ClusterDispatchNodeDto existingNode, ClusterDispatchNodeDto nodeDto) {
    //
    //    if (existingNode != null && existingNode.getClusterNodeHost().equals(nodeDto.getClusterNodeHost())) {
    //        return;
    //    }
    //    //先注册
    //    String newHostName = FolibWsRunManageUtil.getTargetHostName(nodeDto);
    //    //promotionTaskQueue.registerPromotionTaskQueue(newHostName);
    //    promotionTaskQueue.registerV2PromotionTaskQueue(newHostName);
    //    if (existingNode != null) {
    //        //清理之前的
    //        String targetHostName = FolibWsRunManageUtil.getTargetHostName(existingNode);
    //        //promotionTaskQueue.clearPromotionTaskQueue(targetHostName);
    //        promotionTaskQueue.clearV2PromotionTaskQueue(targetHostName);
    //    }
    //}


    // 删除
    @DeleteMapping(value = "/{clusterEnName}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_STORAGE_CONFIGURATION')")
    public ResponseEntity deleteClusterDispatch(
            @PathVariable String clusterEnName,
            @RequestHeader(HttpHeaders.ACCEPT) String accept) {

        final ClusterDispatchNodeDto nodeDto = new ClusterDispatchNodeDto();
        final SyncClusterDispatchDto syncClusterDispatchDto =
                new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.DELETE);

        final ClusterDispatchNodeDto clusterDispatchNodeDto = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().get(clusterEnName);
        if (clusterDispatchNodeDto == null) {
            throw new RuntimeException(String.format("not found ClusterDispatchNode info with clusterEnName %s", clusterEnName));
        }
//            if (clusterDispatchNodeDto.getAutoRegister() != null && clusterDispatchNodeDto.getAutoRegister()) {
//                throw new UnsupportedOperationException("please delete it at the registration node side");
//            }
        try {
            nodeDto.setClusterEnName(clusterEnName);
            clusterDispatchManagementService.deleteClusterNode(nodeDto);
            // 向其他集群节点同步制品分发节点信息
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
            //清理晋级任务队列
            String targetHostName = FolibWsRunManageUtil.getTargetHostName(clusterDispatchNodeDto);
            folibWsRunManageV2.unRegisterSession(targetHostName,"node delete");
            //promotionTaskQueue.clearPromotionTaskQueue(targetHostName);
            //promotionTaskQueue.clearV2PromotionTaskQueue(targetHostName);
            distributionService.clearByNodeTaskQueue(targetHostName);
            return ResponseEntity.ok("ok");
        } catch (Exception e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "修改制品分发节点信息失败", e, accept);
        }
    }

    private void handleWsServer(ClusterDispatchNodeDto nodeDto) {

//        try {
//            folibWsRunManageV2.connectToServerV2(nodeDto);
//        } catch (Exception e) {
//            logger.error("connectToServer Exception",e);
//        }
        Session session = folibWsRunManageV2.getSession(FolibWsRunManageUtil.getTargetHostName(nodeDto));
        if (session != null && session.isOpen()) {
            try {
                session.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        // 向其他集群节点同步同步制品分发节点信息
        SyncClusterDispatchDto syncClusterDispatchDto =
                new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
        clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);

    }

}
