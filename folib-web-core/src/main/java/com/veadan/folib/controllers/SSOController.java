package com.veadan.folib.controllers;

import cn.hutool.jwt.JWTUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.authorization.domain.Client;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.authorization.service.impl.AuthorizationConfigServiceImpl;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.controllers.login.LoginOutput;
import com.veadan.folib.domain.User;
import com.veadan.folib.dto.SSOSessionDto;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.users.domain.SystemRole;
import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.servlet.view.RedirectView;

import javax.inject.Inject;

import javax.ws.rs.ServerErrorException;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.Response;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Slf4j
@Controller
@RequestMapping("/api/sso")
@Api(value = "单点登录", tags = "单点登录客户端管理")
public class SSOController {

    @Inject
    private AuthorizationConfigServiceImpl authorizationConfigService;

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Inject
    protected ConfigurationManager configurationManager;

    /**
     * 单点登录返回的token
     *
     * @param ssoSessionDto 单点登录参数
     * @return token信息
     * @throws Exception 异常
     */
    @PostMapping(value = "/ssoLogin", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity ssoLogin(@RequestBody SSOSessionDto ssoSessionDto) throws Exception {
        Set<Client> clients = authorizationConfigService.getDto().getClients();
        Client clientConfig = Optional.of(clients.stream()
                .filter(r -> r.getClientId()
                        .equalsIgnoreCase(ssoSessionDto.getClientId()))
                .findFirst()).get().orElse(null);
        if (Objects.isNull(clientConfig)) {
            throw new IllegalArgumentException("Param clientId error");
        }
        JSONObject jsonObject = accessToken(ssoSessionDto.getCode(), clientConfig);
        String accessToken = jsonObject.getString("access_token");
        String idToken = jsonObject.getString("id_token");
        if (StringUtils.isNotBlank(accessToken)) {
            String username = handlerUsername(clientConfig, accessToken);
            JSONObject data = new JSONObject();
            data.put("idToken", idToken);
            User user = userService.findByUsername(username);
            if (Objects.isNull(user)) {
                Set<String> roleNames = Sets.newLinkedHashSet();
                roleNames.add(SystemRole.GENERAL.name());
                UserDto userDto = new UserDto();
                userDto.setUsername(username);
                userDto.setRoleNames(roleNames);
                userDto.setPassword("");
                userService.save(new EncodedPasswordUser(userDto, passwordEncoder));
            }
            String token = userService.generateSecurityToken(username);
            data.put("token", token);
            Integer timeout = configurationManager.getSessionTimeoutSeconds();
            //存储JWT令牌到Cookie
            Cookie cookie = new Cookie(JwtTokenFetcher.AUTHORIZATION_COOKIE, token);
            cookie.setPath("/");
            cookie.setMaxAge(timeout);
            HttpServletResponse response = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getResponse();
            response.addCookie(cookie);
            return ResponseEntity.ok().body(new LoginOutput(token, idToken));
        } else {
            throw new IllegalArgumentException("Error in obtaining accessToken");
        }
    }

    /**
     * 单点登录返回的token
     *
     * @param code  授权码
     * @param state 单点登录客户端ID
     * @return token信息
     * @throws Exception 异常
     */

    @AuditLog(value = AuditEventNameEnum.SSO_LOGIN,target ="#clientId" )
    @GetMapping(value = "/login/{clientId}")
    public RedirectView login(@RequestParam(name = "code") String code, @RequestParam(name = "state", required = false) String state, @PathVariable("clientId") String clientId) throws Exception {
        log.info("Params code [{}] state [{}] clientId [{}]", code, state, clientId);
        Set<Client> clients = authorizationConfigService.getDto().getClients();
        Client clientConfig = Optional.of(clients.stream()
                .filter(r -> r.getClientId()
                        .equalsIgnoreCase(clientId))
                .findFirst()).get().orElse(null);
        if (Objects.isNull(clientConfig)) {
            throw new IllegalArgumentException("Param clientId error");
        }
        JSONObject jsonObject = accessToken(code, clientConfig);
        String accessToken = jsonObject.getString("access_token");
        String idToken = jsonObject.getString("id_token");
        if (StringUtils.isNotBlank(accessToken)) {
            String username = handlerUsername(clientConfig, accessToken);
            JSONObject data = new JSONObject();
            data.put("idToken", idToken);
            User user = userService.findByUsername(username);
            if (Objects.isNull(user)) {
                Set<String> roleNames = Sets.newLinkedHashSet();
                roleNames.add(SystemRole.GENERAL.name());
                UserDto userDto = new UserDto();
                userDto.setUsername(username);
                userDto.setRoleNames(roleNames);
                userDto.setPassword("");
                userService.save(new EncodedPasswordUser(userDto, passwordEncoder));
            }
            String token = userService.generateSecurityToken(username);
            data.put("token", token);
            Integer timeout = configurationManager.getSessionTimeoutSeconds();
            //存储JWT令牌到Cookie
            Cookie cookie = new Cookie(JwtTokenFetcher.AUTHORIZATION_COOKIE, token);
            cookie.setPath("/");
            cookie.setMaxAge(timeout);
            HttpServletResponse response = ((ServletRequestAttributes) (RequestContextHolder.currentRequestAttributes())).getResponse();
            response.addCookie(cookie);
            return new RedirectView(getSSOIndex(token, idToken), true, false);
        } else {
            throw new IllegalArgumentException("Error in obtaining accessToken");
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
    @AuditLog(value = AuditEventNameEnum.SSO_SETTING,target =" '新增clientId:'+#client.clientId" )
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @PostMapping(value = "/addClient", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity addClient(@RequestBody Client client) throws IOException {
        AuthorizationConfigDto authorizationConfigDto = authorizationConfigService.getDto();
        Set<Client> clients = authorizationConfigDto.getClients();
        boolean exist = clients.stream().anyMatch(s -> s.getClientId().equals(client.getClientId()));
        if (!exist) {
            authorizationConfigService.addClient(client);
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
        return ResponseEntity.ok(client);
    }

    @ApiOperation(value = "delete sso clients.")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "")})
    @AuditLog(value = AuditEventNameEnum.SSO_SETTING,target ="'删除clientId:'+#clientId" )
    @GetMapping(value = "/deleteClient/{clientId}", produces = {MediaType.APPLICATION_JSON_VALUE})
    @ResponseBody
    public ResponseEntity delClient(@PathVariable(name = "clientId") String clientId) throws Exception {
        authorizationConfigService.deleteClient(clientId);
        return ResponseEntity.ok(clientId);
    }


    /**
     * 获取accessToken
     *
     * @param code   授权码
     * @param client sso配置
     * @return accessToken
     */
    private JSONObject accessToken(String code, Client client) {
        MultivaluedHashMap<String, String> map = new MultivaluedHashMap<String, String>();
        map.add("client_id", client.getClientId());
        Map<String, String> headerMap = Maps.newHashMap();
        if (StringUtils.isNotBlank(client.getClientSecret())) {
            map.add("client_secret", client.getClientSecret());
            headerMap.put("Authorization", "Basic " + Base64.getEncoder().encodeToString(String.format("%s:%s", client.getClientId(), client.getClientSecret()).getBytes(StandardCharsets.UTF_8)));
        }
        map.add("code", code);
        map.add("scope", "openid profile email");
        map.add("grant_type", "authorization_code");
        map.add("redirect_uri", client.getRedirectPath());
        headerMap.put("Content-Type", "application/x-www-form-urlencoded");
        Response response = null;
        try {
            response = doPostForm(client.getAccessTokenUrl(), map, headerMap);
            String json = response.readEntity(String.class);
            log.info("AccessToken result [{}]", json);
            if (response.getStatus() != HttpStatus.SC_OK) {
                throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                        Response.Status.INTERNAL_SERVER_ERROR);
            } else {
                return JSONObject.parseObject(json);
            }
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
    }

    /**
     * 发送post请求
     *
     * @param url       url
     * @param map       map
     * @param headerMap headerMap
     * @return 响应
     */
    private Response doPostForm(String url, MultivaluedHashMap<String, String> map, Map<String, String> headerMap) {
        javax.ws.rs.client.Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Invocation.Builder builder = target.request();
        for (Map.Entry<String, String> entry : headerMap.entrySet()) {
            builder = builder.header(entry.getKey(), entry.getValue());
        }
        return builder.post(Entity.form(map));
    }

    /**
     * 获取SSO 登录用户名
     *
     * @param client      sso配置
     * @param accessToken accessToken
     * @return 用户名
     */
    private String handlerUsername(Client client, String accessToken) {
        cn.hutool.jwt.JWT jwt = JWTUtil.parseToken(accessToken);
        String username = "";
        Object preferredUsername = jwt.getPayload().getClaim("preferred_username");
        //优先使用preferredUsername
        if (Objects.nonNull(preferredUsername)) {
            username = preferredUsername.toString();
        }
        //如果另外设置了用户名字段，则使用用户名字段对应的值
        if (StringUtils.isNotBlank(client.getUserInfoUrl()) && StringUtils.isNotBlank(client.getUsername())) {
            Map<String, String> headerMap = Maps.newHashMap();
            headerMap.put("Authorization", "Bearer " + accessToken);
            Response response = null;
            try {
                response = doGet(client.getUserInfoUrl(), headerMap);
                String userInfo = response.readEntity(String.class);
                log.info("UserInfo result [{}]", userInfo);
                if (response.getStatus() != HttpStatus.SC_OK) {
                    throw new ServerErrorException(response.getStatus() + " | Unable to greet()",
                            Response.Status.INTERNAL_SERVER_ERROR);
                }
                JSONObject userInfoJson = JSONObject.parseObject(userInfo);
                if (userInfoJson.containsKey(client.getUsername())) {
                    String realUsername = userInfoJson.getString(client.getUsername());
                    if (StringUtils.isNotBlank(realUsername)) {
                        username = realUsername;
                    }
                }
            } finally {
                if (Objects.nonNull(response)) {
                    response.close();
                }
            }
        }
        if (StringUtils.isBlank(username)) {
            throw new IllegalArgumentException("SSO username not found");
        }
        return username;
    }

    /**
     * 发送post请求
     *
     * @param url       url
     * @param headerMap headerMap
     * @return 响应
     */
    private Response doGet(String url, Map<String, String> headerMap) {
        javax.ws.rs.client.Client client = clientPool.getRestClient();
        WebTarget target = client.target(url);
        Invocation.Builder builder = target.request();
        for (Map.Entry<String, String> entry : headerMap.entrySet()) {
            builder = builder.header(entry.getKey(), entry.getValue());
        }
        return builder.get();
    }

    private String getSSOIndex(String token, String idToken) {
        String webUrlPrefix = System.getProperty(GlobalConstants.WEB_URL_PREFIX);
        if (StringUtils.isBlank(webUrlPrefix)) {
            webUrlPrefix = "/ui/";
        }
        return webUrlPrefix + "index.html#/sso?token=" + token + "&idToken=" + idToken;
    }

}
