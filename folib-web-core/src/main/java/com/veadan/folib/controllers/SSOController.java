package com.veadan.folib.controllers;

import cn.hutool.jwt.JWTUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Sets;
import com.veadan.folib.authorization.domain.Client;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.service.impl.AuthorizationConfigServiceImpl;
import com.veadan.folib.cluster.SyncAuthorizationEnum;
import com.veadan.folib.controllers.cluster.dto.SyncAuthorizationDto;
import com.veadan.folib.domain.User;
import com.veadan.folib.dto.SSOsessionDto;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.ws.rs.ServerErrorException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.util.Set;


@Controller
@RequestMapping("/api/sso")
@Api(value = "keycloak 单点的登录", tags = "keycloak 单点的登录客户端管理")
public class SSOController {

    @Inject
    private AuthorizationConfigServiceImpl authorizationConfigService;

    @Inject
    @DatabaseUserService.Database
    private UserService userService;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Inject
    private ClusterSyncService clusterSyncService;


    /**
     * 单点登录返回的token
     *
     * @param ssOsessionDto
     * @return
     * @throws Exception
     */
    @ApiOperation(value = "ssoLogin.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PostMapping(value = "/ssoLogin", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity ssoLogin(@RequestBody SSOsessionDto ssOsessionDto) throws Exception {
        javax.ws.rs.client.Client client = clientPool.getRestClient();
        WebTarget resource = client.target(ssOsessionDto.getAccess_token_url());
        MultivaluedHashMap<String, String> map = new MultivaluedHashMap();
        map.add("client_id", ssOsessionDto.getClient_id());
        map.add("code", ssOsessionDto.getCode());
        map.add("grant_type", ssOsessionDto.getGrant_type());
        map.add("redirect_uri", ssOsessionDto.getRedirect_uri());
        Response response = resource.request().header("Content-Type", "application/x-www-form-urlencoded").post(Entity.form(map));

        if (response.getStatus() != HttpStatus.SC_OK) {
            throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                    Response.Status.INTERNAL_SERVER_ERROR);
        } else {
            String json = response.readEntity(String.class);
            JSONObject jsonObject = JSONObject.parseObject(json);
            String accessToken = jsonObject.getString("access_token");
            if (!StringUtils.isEmpty(accessToken)) {
                cn.hutool.jwt.JWT jwt = JWTUtil.parseToken(accessToken);
                String username = jwt.getPayload().getClaim("preferred_username").toString();
                User user = userService.findByUsername(username);
                if (user == null) {
                    Set<String> roleNames = Sets.newLinkedHashSet();
                    roleNames.add(SystemRole.GENERAL.name());
                    UserDto userDto = new UserDto();
                    userDto.setUsername(username);
                    userDto.setPassword("guest");
                    userDto.setRoleNames(roleNames);
                    userService.save(new EncodedPasswordUser(userDto, passwordEncoder));
                    // 直接调用login的逻辑返回
                    return ResponseEntity.status(200).body(userDto);
                } else {
                    // 直接返回login的结果
                    return ResponseEntity.status(200).body(user);
                }
            } else {
                throw new Exception("非法用户！");
            }
        }
    }

    @ApiOperation(value = "Used to search for sso clients.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @GetMapping(value = "/getClients", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity getAllClients() {
        Set<Client> clients = authorizationConfigService.getDto().getClients();
        return ResponseEntity.status(200).body(clients);
    }

    @ApiOperation(value = "add sso clients.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PostMapping(value = "/addClient", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity addClient(@RequestBody Client client) throws IOException {
        AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
        Set<Client> clients = authorizationConfigDto.getClients();
        boolean exist = clients.stream().anyMatch(s -> s.getClientId().equals(client.getClientId()));
        if (!exist) {
            authorizationConfigService.addClient(client);
            syncAuthorizationConfig();
            return ResponseEntity.ok(client);
        } else {
            throw new RuntimeException("clientId已存在，不能重复添加！");
        }
    }

    @ApiOperation(value = "update sso clients.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PostMapping(value = "/updateClient", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity updateClient(@RequestBody Client client) throws Exception {
        authorizationConfigService.deleteClient(client.getClientId());
        authorizationConfigService.addClient(client);
        syncAuthorizationConfig();
        return ResponseEntity.ok(client);
    }

    @ApiOperation(value = "delete sso clients.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @GetMapping(value = "/deleteClient/{clientId}", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity delClient(@PathVariable(name = "clientId") String clientId) throws Exception {
        authorizationConfigService.deleteClient(clientId);
        syncAuthorizationConfig();
        return ResponseEntity.ok(clientId);
    }

    private void syncAuthorizationConfig() {
        AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
        SyncAuthorizationDto syncAuthorizationDto = new SyncAuthorizationDto(authorizationConfigDto, SyncAuthorizationEnum.UPDATE);
        clusterSyncService.syncAuthorization(syncAuthorizationDto);
    }
}
