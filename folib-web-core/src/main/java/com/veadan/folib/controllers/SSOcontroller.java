package com.veadan.folib.controllers;

import cn.hutool.jwt.JWTUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.authorization.domain.Client;
import com.veadan.folib.authorization.service.impl.AuthorizationConfigServiceImpl;
import com.veadan.folib.domain.User;
import com.veadan.folib.dto.SSOsessionDto;

import com.veadan.folib.users.dto.UserDto;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.DatabaseUserService;
import com.veadan.folib.users.service.impl.EncodedPasswordUser;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Controller;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;

import javax.inject.Inject;
import java.io.IOException;

import java.util.HashSet;
import java.util.Set;


@Controller
@RequestMapping("/api/sso")
@Api(value = "/api/sso")
public class SSOcontroller {
    @Inject
    private AuthorizationConfigServiceImpl authorizationConfigService;

    @Inject
    @DatabaseUserService.Database
    private UserService userService;


    @Inject
    private RestTemplate restTemplate;


    @Inject
    private PasswordEncoder passwordEncoder;




    // 单点登录返回的token
    @ApiOperation(value = "ssoLogin.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @PostMapping(value = "/ssoLogin", produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity ssoLogin( @RequestBody SSOsessionDto ssOsessionDto)throws Exception{
            MultiValueMap<String,String> map=new LinkedMultiValueMap();
            map.add("client_id",ssOsessionDto.getClient_id());
            map.add("code",ssOsessionDto.getCode());
            map.add("grant_type",ssOsessionDto.getGrant_type());
            map.add("redirect_uri",ssOsessionDto.getRedirect_uri());
            HttpHeaders headers = new HttpHeaders();
            headers.add("Content-Type", "application/x-www-form-urlencoded");
        HttpEntity<MultiValueMap<String, Object>> request = new HttpEntity(map,headers);
        String json = restTemplate.postForObject(ssOsessionDto.getAccess_token_url(), request, String.class);
        JSONObject jsonObject=JSONObject.parseObject(json);
        String accessToken = jsonObject.getString("access_token");
        if(!StringUtils.isEmpty(accessToken)){
            cn.hutool.jwt.JWT jwt= JWTUtil.parseToken(accessToken);
            String username=jwt.getPayload().getClaim("preferred_username").toString();
            User user = userService.findByUsername(username);
            Set<String> set=new HashSet<>();
            set.add("GENERAL");
        if(user==null){
        UserDto userDto=new UserDto();
        userDto.setUsername(username);
        userDto.setPassword("guest");
        userDto.setRoleNames(set);
        userService.save(new EncodedPasswordUser(userDto, passwordEncoder));
        // 直接调用login的逻辑返回
            return ResponseEntity.status(200).body(userDto );
        }else {
         // 直接返回login的结果
            return ResponseEntity.status(200).body(user );
        }


        }else {
            throw new Exception("非法用户！");
        }


    }

    @ApiOperation(value = "Used to search for sso clients.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @GetMapping(value = "/getClients", produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity getAllClinets(){
        Set<Client> clients= authorizationConfigService.getDto().getClients();
       return ResponseEntity.status(200).body(clients);

    }

    @ApiOperation(value = "add sso clients.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @PostMapping(value = "/addClient", produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity addClient(@RequestBody Client client) throws IOException {
       Set<Client>  clients= authorizationConfigService.getDto().getClients();
       Boolean exsit=   clients.stream().anyMatch(s->{
           return s.getClientId().equals(client.getClientId());
       });
       if(!exsit){
           authorizationConfigService.addClient(client);
           return ResponseEntity.ok(client);
       }else {
           throw new RuntimeException("clientId已存在，不能重复添加！");
       }

    }

    @ApiOperation(value = "update sso clients.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @PostMapping(value = "/updateClient", produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity updateClient(@RequestBody Client client)throws Exception{
       authorizationConfigService.deleteClient(client.getClientId());
       authorizationConfigService.addClient(client);
        return ResponseEntity.ok(client);

    }

    @ApiOperation(value = "delete sso clients.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @GetMapping(value = "/deleteClient/{clientId}", produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity delClient( @PathVariable(name = "clientId") String clientId) throws Exception{
        authorizationConfigService.deleteClient(clientId);
        return ResponseEntity.ok(clientId);

    }
}
