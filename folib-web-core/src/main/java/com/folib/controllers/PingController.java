/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.controllers;

import com.hazelcast.core.HazelcastInstance;
import com.folib.entity.Dict;
import com.folib.forms.dict.DictForm;
import com.folib.services.DictService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.Date;
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
    private DictService dictService;

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

    @ApiResponses(value = { @ApiResponse(code = 200, message = "Successful machineCode") })
    @GetMapping("/testConnect/{time}")
    public ResponseEntity<Object> testConnect(@PathVariable Long time){
        Dict dict = Dict.builder().dictType("test").dictKey("abc").createTime(new Date()).comment("comment").build();
        dictService.saveDict(dict);
        try {
            Thread.sleep(time);
        } catch (Exception ignore) {

        }
        DictForm dictForm = DictForm.builder().id(dict.getId()).dictKey("def").build();
        dictService.updateDict(dictForm);
        return ResponseEntity.ok().build();
    }





}
