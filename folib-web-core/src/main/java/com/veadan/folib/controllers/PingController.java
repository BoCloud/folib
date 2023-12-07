package com.veadan.folib.controllers;

import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.data.CacheName;
import com.veadan.folib.licence.ActivateVo;
import com.veadan.folib.licence.MacUtil;
import com.veadan.folib.services.CodeActivateService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.concurrent.ConcurrentMap;

/**
 * @author Steve Todorov
 */
@Controller
@RequestMapping("/api/ping")
@Api(description = "ping服务管理",tags = "ping服务管理")
public class PingController
        extends BaseController


{

    @Inject
    private HazelcastInstance hazelcastInstance;

    private ConcurrentMap<String,String> retrieveMap() {
        return hazelcastInstance.getMap("map");
    }

    @Inject
    private CodeActivateService codeActivateService;

    static final String READY_STREAM_VALUE = "event:ready\ndata: \n\n";

    @ApiResponses(value = { @ApiResponse(code = 200, message = "Folib is up and working.") })
    @GetMapping(produces = { MediaType.TEXT_EVENT_STREAM_VALUE + ";charset=UTF-8" })
    public ResponseEntity ping()
    {
        return ResponseEntity.ok().header(HttpHeaders.TRANSFER_ENCODING, "chunked").body(READY_STREAM_VALUE);
    }

    /**
     * This endpoint is used in the frontend to check if a token is valid when the SPA has been loaded for the first time
     * and there was a token stored in the client's browser.
     *
     * @param accept
     *
     * @return ResponseEntity
     */
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful when token is still valid.") })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping(value = "/token",
                produces = { MediaType.APPLICATION_JSON_VALUE,
                             MediaType.TEXT_PLAIN_VALUE })
    public ResponseEntity protectedPing(@RequestHeader(HttpHeaders.ACCEPT) String accept)
    {
        return getSuccessfulResponseEntity("pong", accept);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful activated") })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping("/activate")
    public ResponseEntity activate(@RequestParam("key") String key,@RequestParam("isPoc") boolean isPoc){
        try {
            return ResponseEntity.ok().body(codeActivateService.activate(key,isPoc));

        } catch (Exception e) {
            return ResponseEntity.status(500).body("获取机器码异常");
//            throw new RuntimeException(e);
        }
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful machineCode") })
    @PreAuthorize("hasAuthority('AUTHENTICATED_USER')")
    @GetMapping("/machineCode")
    public ResponseEntity machineCode(){

        try {
            String mac= MacUtil.getMachineCode();


            return ResponseEntity.ok().body(mac);

        } catch (Exception e) {
            return ResponseEntity.status(500).body("获取机器码异常");
//            throw new RuntimeException(e);
        }

    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful machineCode") })
    @GetMapping("/checkMachineCode")
    public ResponseEntity checkMachineCode(){

        try {
            ActivateVo activateVo = codeActivateService.isNotActivate();


            return ResponseEntity.ok().body(activateVo);

        } catch (Exception e) {
            return ResponseEntity.status(500).body("获取机器信息异常");
//            throw new RuntimeException(e);
        }

    }


    // 使用 @CachePut 更新缓存
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful machineCode") })
    @PostMapping("/saveTest/{name}")
    public ResponseEntity saveTest(@PathVariable String name, @RequestBody String data){

        retrieveMap().put(name, data);
        // 这里应该有一些逻辑来处理数据并保存
        return ResponseEntity.ok().body(data);
    }




    // 使用 @Cacheable 读取缓存
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful machineCode") })
    @GetMapping("/getTest/{name}")
    public ResponseEntity getTest(@PathVariable String name){
        String value = retrieveMap().get(name);
        return ResponseEntity.ok().body(value);
    }





}
