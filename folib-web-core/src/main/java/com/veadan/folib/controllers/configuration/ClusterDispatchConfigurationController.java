package com.veadan.folib.controllers.configuration;


import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpUtil;
import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.controllers.cluster.dto.SyncClusterDispatchDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.forms.configuration.ClusterDispatchNodeForm;
import com.veadan.folib.scanner.common.exception.BusinessException;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import com.veadan.folib.ws.server.Command;
import com.veadan.folib.ws.server.WSMessageRequest;
import com.veadan.folib.ws.server.handler.command.FolibWsServerDeleteNodeInfoCommand;
import com.veadan.folib.ws.server.handler.command.FolibWsServerSaveNodeInfoCommand;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
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

import javax.websocket.DeploymentException;
import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;


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
            String targetHostName = folibWsRunManageV2.getTargetHostName(nodeDto);
            Session session = folibWsRunManageV2.getSession(targetHostName);
            nodeDto.setWsClientOnline(session!=null&&session.isOpen());
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
        try {
            // 创建分发节点
            ClusterDispatchNodeDto nodeDto = new ClusterDispatchNodeDto();
            BeanUtils.copyProperties(clusterDispatchNodeForm, nodeDto);
            nodeDto.setCreateTime(LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
            clusterDispatchManagementService.createClusterNode(nodeDto);

            // 向其他集群节点同步同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);

            handleWsServer(clusterDispatchNodeForm, nodeDto);
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

        try {
            ClusterDispatchNodeDto nodeDto = new ClusterDispatchNodeDto();
            BeanUtils.copyProperties(clusterDispatchNodeForm, nodeDto);
            clusterDispatchManagementService.createClusterNode(nodeDto);

            // 向其他集群节点同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);

            handleWsServer(clusterDispatchNodeForm, nodeDto);
            return getSuccessfulResponseEntity("ok", accept);
        } catch (Exception e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "修改制品分发节点信息失败", e, accept);
        }
    }

    // 删除
    @DeleteMapping(value = "/{clusterEnName}",
            produces = {MediaType.TEXT_PLAIN_VALUE,
                    MediaType.APPLICATION_JSON_VALUE})
    @PreAuthorize("hasAuthority('CONFIGURATION_DELETE_STORAGE_CONFIGURATION')")
    public ResponseEntity deleteClusterDispatch(
            @PathVariable String clusterEnName,
            @RequestHeader(HttpHeaders.ACCEPT) String accept) {
        try {
            final ClusterDispatchNodeDto nodeDto = new ClusterDispatchNodeDto();
            final SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.DELETE);

            // 通知WsServer删除节点信息 & 断开与WsServer的连接
            // - 获取与WsServer通信会话
            final ClusterDispatchNodeDto clusterDispatchNodeDto = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().get(clusterEnName);
            out:
            if (null != clusterDispatchNodeDto) {
                if (StrUtil.isNotBlank(clusterDispatchNodeDto.getClusterNodeDesc()) &&
                        !(clusterDispatchNodeDto.getClusterNodeDesc().contains("【自动注册节点】"))) {
                    break out;
                }

                final String baseUrl = configurationManager.getConfiguration().getBaseUrl();
                final String clusterNodeHost = clusterDispatchNodeDto.getClusterNodeHost();
                final String originHost = UrlUtils.getHost(baseUrl);
                final Integer originPort = UrlUtils.getPort(baseUrl);
                final String destHost = UrlUtils.getHost(clusterNodeHost);
                final Integer destPort = UrlUtils.getPort(clusterNodeHost);
                final String originNodeName = String.format("%s:%s", originHost, originPort);
                final String destNodeName = String.format("%s:%s", destHost, destPort);
                final FolibWsClientRunManage.FolibWsServerRun wsServerRun = FolibWsClientRunManage.getWsServerRun(destNodeName);
                // 远程对应节点名称是：originNodeName
                if (null != wsServerRun) {
                    syncClusterDispatchDto.getNodeDto().setClusterEnName(originNodeName);
                    final FolibWsAction folibWsAction = new FolibWsAction()
                            .command(FolibWsServerDeleteNodeInfoCommand.COMMAND)
                            .payload(syncClusterDispatchDto);
                    wsServerRun.doAction(folibWsAction);
                    FolibWsClientRunManage.remove(destNodeName);
                }
            }

            nodeDto.setClusterEnName(clusterEnName);
            clusterDispatchManagementService.deleteClusterNode(nodeDto);
            // 向其他集群节点同步制品分发节点信息
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);

            return ResponseEntity.ok("ok");
        } catch (Exception e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "修改制品分发节点信息失败", e, accept);
        }
    }

    private void handleWsServer(ClusterDispatchNodeForm clusterDispatchNodeForm, ClusterDispatchNodeDto nodeDto) {
        // 连接到WsServer
        final String clusterNodeHost = nodeDto.getClusterNodeHost();
        final String baseUrl = configurationManager.getConfiguration().getBaseUrl();
        final String originHost = UrlUtils.getHost(baseUrl);
        final Integer originPort = UrlUtils.getPort(baseUrl);
        final String destHost = UrlUtils.getHost(clusterNodeHost);
        final Integer destPort = UrlUtils.getPort(clusterNodeHost);
        final String destNodeName = String.format("%s", destHost);
///            final String destNodeName = String.format("%s", destHost, destPort);
        final String originNodeName = String.format("%s:%s", originHost, originPort);
        final String destUri = String.format("/wsv2/folib/%s", originNodeName);
        final boolean enableSSL = HttpUtil.isHttps(clusterNodeHost);


        String uri = String.format("%s://%s:%s", enableSSL ? "wss" : "ws", destHost, destPort + destUri);

        String targetHostName = folibWsRunManageV2.getTargetHostName(nodeDto);

        try {
            folibWsRunManageV2.connectToServer(targetHostName,uri);
        } catch (DeploymentException | IOException e) {
            throw new RuntimeException(e);
        }

        // 向其他集群节点同步同步制品分发节点信息
        SyncClusterDispatchDto syncClusterDispatchDto =
                new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
        clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);

        // 向WsServer发送创建节点维护信息

        final ClusterDispatchNodeDto registerNodeInfoDto = new ClusterDispatchNodeDto();
        BeanUtils.copyProperties(clusterDispatchNodeForm, registerNodeInfoDto);
        registerNodeInfoDto.setAutoRegister(true);
        registerNodeInfoDto.setClusterNodeHost(baseUrl);
        registerNodeInfoDto.setClusterEnName(originNodeName);
        registerNodeInfoDto.setClusterCnName(String.format("【自动注册节点】%s", originNodeName));
        registerNodeInfoDto.setClusterNodeDesc(String.format("【自动注册节点】禁止操作，此节点信息是由客户端节点（%s）向当前节点（%s）发起注册生成", originNodeName, destNodeName));

        FolibWsServerSaveNodeInfoCommand.Payload payload = new FolibWsServerSaveNodeInfoCommand.Payload(registerNodeInfoDto,
                SyncClusterDispatchEnum.ADD_OR_UPDATE);
        try {
            folibWsRunManageV2.sendRequest(targetHostName,new WSMessageRequest(Command.SERVER_INFO, payload));
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            throw new RuntimeException(e);
        }

    }

}
