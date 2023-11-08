package com.veadan.folib.controllers.configuration;


import com.veadan.folib.cluster.SyncClusterDispatchEnum;
import com.veadan.folib.controllers.cluster.dto.SyncClusterDispatchDto;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.forms.configuration.ClusterDispatchNodeForm;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.validation.RequestBodyValidationException;
import com.veadan.folib.ws.client.manage.FolibWsServerRunManage;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.server.handler.command.FolibWsServerSaveNodeInfoCommand;
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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.socket.TextMessage;

import java.net.URL;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
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
        return ResponseEntity.ok(map.values());
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

            // 连接到WsServer
            final String clusterNodeHost = nodeDto.getClusterNodeHost();
            final String baseUrl = configurationManager.getConfiguration().getBaseUrl();
            final URL destUrl = new URL(clusterNodeHost);
            final URL originUrl = new URL(baseUrl);
            final String originHost = originUrl.getHost();
            final Integer originPort = UrlUtils.getPort(originUrl.toString());
            final String destHost = destUrl.getHost();
            final Integer destPort = UrlUtils.getPort(clusterNodeHost);
            final String destNodeName = String.format("%s:%s", destHost, destPort);
            final String originNodeName = String.format("%s:%s", originHost, originPort);
            final String destUri = String.format("/ws/folib/%s", originNodeName);
            FolibWsServerRunManage.up(destNodeName, destHost, destPort, destUri, true);

            // 向WsServer发送创建节点维护信息
            final FolibWsServerRunManage.FolibWsServerRun wsServerRun = FolibWsServerRunManage.getWsServerRun(destNodeName);
            if (null != wsServerRun) {
                final ClusterDispatchNodeDto registerNodeInfoDto = new ClusterDispatchNodeDto();
                BeanUtils.copyProperties(clusterDispatchNodeForm, registerNodeInfoDto);
                registerNodeInfoDto.setClusterNodeHost(baseUrl);
                registerNodeInfoDto.setClusterEnName(originNodeName);
                registerNodeInfoDto.setClusterCnName(String.format("【自动注册节点】%s", originNodeName));
                registerNodeInfoDto.setClusterNodeDesc(String.format("【自动注册节点】次节点信息是由客户端节点（%s）向服务端节点（%s）发起注册生成", originNodeName, destNodeName));
                final FolibWsAction folibWsAction = new FolibWsAction()
                        .setCommand(FolibWsServerSaveNodeInfoCommand.COMMAND)
                        .setPayload(
                                new FolibWsServerSaveNodeInfoCommand.Payload(registerNodeInfoDto,
                                        SyncClusterDispatchEnum.ADD_OR_UPDATE).encode()
                        );
                if (wsServerRun.getSession().isOpen()) {
                    wsServerRun.getSession().sendMessage(new TextMessage(folibWsAction.encode()));
                }
            }

            // 向其他集群节点同步同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
            return getSuccessfulResponseEntity("ok", accept);
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

            // 

            // 向其他集群节点同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.ADD_OR_UPDATE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
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
            ClusterDispatchNodeDto nodeDto = new ClusterDispatchNodeDto();
            nodeDto.setClusterEnName(clusterEnName);
            clusterDispatchManagementService.deleteClusterNode(nodeDto);

            // 向其他集群节点同步制品分发节点信息
            SyncClusterDispatchDto syncClusterDispatchDto =
                    new SyncClusterDispatchDto(nodeDto, SyncClusterDispatchEnum.DELETE);
            clusterSyncService.syncClusterDispatch(syncClusterDispatchDto);
            return ResponseEntity.ok("ok");
        } catch (Exception e) {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, "修改制品分发节点信息失败", e, accept);
        }
    }


}
