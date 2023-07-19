package com.veadan.folib.controllers;

import com.veadan.folib.authorization.domain.Client;
import com.veadan.folib.authorization.service.impl.AuthorizationConfigServiceImpl;
import com.veadan.folib.storage.search.SearchResults;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.json.JSONObject;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.io.IOException;
import java.util.Set;

@Controller
@RequestMapping("/api/sso")
@Api(value = "/api/sso")
public class SSOcontroller {
    @Inject
    private AuthorizationConfigServiceImpl authorizationConfigService;

    // 单点登录返回的token
    @ApiOperation(value = "Used to search for sso clients.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "") })
    @GetMapping(value = "/login/{token}", produces = { MediaType.APPLICATION_JSON_VALUE })
    @ResponseBody
    public ResponseEntity ssoLogin(@PathVariable(name = "token") String token){
        return ResponseEntity.status(200).body(token);
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
