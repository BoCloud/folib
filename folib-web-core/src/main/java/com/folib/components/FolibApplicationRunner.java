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
package com.folib.components;

import com.folib.cluster.ClusterProperties;

import com.folib.components.cassandra.CassandraComponent;
import com.folib.config.janusgraph.JanusGraphDbProfile;
import com.folib.entity.Dict;
import com.folib.enums.DictTypeEnum;
import com.folib.enums.UpgradeTaskStatusEnum;
import com.folib.scanner.common.util.DateUtils;
import com.folib.scanner.common.util.SpringContextUtil;
import com.folib.scanner.service.SbomScannerService;
import com.folib.services.DictService;
import com.folib.services.NodeService;
import jakarta.annotation.Resource;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.web.context.WebServerInitializedEvent;

import org.springframework.boot.web.embedded.undertow.UndertowWebServer;
import org.springframework.boot.web.server.WebServer;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.*;

/**
 * @author leipenghui
 * @date 2023/3/24
 **/
@Slf4j
@Component
public class FolibApplicationRunner implements ApplicationRunner, ApplicationListener<WebServerInitializedEvent> {

    @Resource
    private SbomScannerService scanService;

    @Autowired
    private NodeService nodeService;

    @Autowired
    private DictService dictService;

    @Autowired
    @Lazy
    private DistributedCacheComponent distributedCacheComponent;

    @Autowired
    private ClusterProperties clusterProperties;



    @Override
    public void run(ApplicationArguments args) throws Exception {
        this.initData();
        //jetty();

    }

    /**
     * 初始化数据
     */
    private void initData() {
        initSystemPropertiesData();
        Dict existsDict = dictService.selectLatestOneDict(Dict.builder().dictType(DictTypeEnum.INITIAL_INITIALIZATION.getType()).build());
        boolean isInit = Objects.isNull(existsDict);
        log.info("Is this the first initialization [{}]", isInit);
        if (isInit) {
            if (JanusGraphDbProfile.PROFILE_EMBEDDED.equals(System.getProperty(JanusGraphDbProfile.PROPERTY_PROFILE))) {
                String clusterNodeTotal = System.getProperty("CLUSTER_NODE_TOTAL");
                if (StringUtils.isNotBlank(clusterNodeTotal)) {
                    log.info("Modify the cassandra replication factor [{}]", clusterNodeTotal);
                    nodeService.modifyReplicationFactor(Integer.parseInt(clusterNodeTotal));
                }
            }
            dictService.saveOrUpdateDict(Dict.builder().dictType(DictTypeEnum.INITIAL_INITIALIZATION.getType()).dictKey(DateUtils.formatTime(new Date())).build(), true);
        }
        String gcGraceSeconds = System.getProperty(CassandraComponent.GC_GRACE_SECONDS_KEY, CassandraComponent.DEFAULT_GC_GRACE_SECONDS.toString());
        nodeService.modifyGcGraceSeconds(Integer.parseInt(gcGraceSeconds));
        handlerUnExecutedTask();
        boolean enableAnalysis = Boolean.parseBoolean(System.getenv("folib.enableAnalysis"));
        if (scanService.countProperties() <= 1 && enableAnalysis) {
            //初始化漏洞库，耗时长，放到最后
            log.info("The initialization of vulnerability data begins ");
            scanService.updateMirror();
            log.info("The initialization of vulnerability data ends ");
        }
    }

    /**
     * 处理升级未执行的任务
     */
    private void handlerUnExecutedTask() {
        List<Dict> dictList = dictService.selectUnExecutedTask();
        if (CollectionUtils.isNotEmpty(dictList)) {
            String methodKey = "method", methodName;
            String[] arr;
            Class clazz;
            Object proxyObject;
            Method targetMethod;
            for (Dict dict : dictList) {
                if (methodKey.equals(dict.getDictKey()) && !UpgradeTaskStatusEnum.EXECUTING.getStatus().equals(dict.getComment())) {
                    try {
                        arr = dict.getDictValue().split("@");
                        clazz = Class.forName(arr[0]);
                        proxyObject = SpringContextUtil.getBean(clazz);
                        methodName = arr[1];
                        // 获取代理对象执行的方法
                        targetMethod = getMethod(proxyObject.getClass(), methodName);
                        if (Objects.nonNull(targetMethod)) {
                            // 执行方法
                            if (StringUtils.isNotBlank(dict.getAlias())) {
                                dict.setComment(UpgradeTaskStatusEnum.EXECUTING.getStatus());
                                dictService.updateUnExecutedTask(dict);
                                targetMethod.invoke(proxyObject, dict.getAlias());
                            } else {
                                dict.setComment(UpgradeTaskStatusEnum.EXECUTING.getStatus());
                                dictService.updateUnExecutedTask(dict);
                                targetMethod.invoke(proxyObject);
                            }
                            log.info("执行升级task [{}] [{}]", clazz, methodName);
                            dict.setComment(UpgradeTaskStatusEnum.EXECUTED_SUCCESS.getStatus());
                            dictService.updateUnExecutedTask(dict);
                        }
                    } catch (Exception ex) {
                        ex.printStackTrace();
                        dict.setComment(UpgradeTaskStatusEnum.EXECUTED_FAIL.getStatus());
                        dictService.updateUnExecutedTask(dict);
                        log.error("执行升级task错误 [{}] [{}]", dict.getDictValue(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            }
        }
    }

    /**
     * 获取目标方法
     *
     * @param proxyObject proxyObject
     * @param methodStr   methodStr
     * @return 目标方法
     */
    private Method getMethod(Class<?> proxyObject, String methodStr) {
        Method[] methods = proxyObject.getMethods();
        for (Method method : methods) {
            if (method.getName().equalsIgnoreCase(methodStr)) {
                return method;
            }
        }
        return null;
    }

    /**
     * 初始化环境参数数据
     */
    private void initSystemPropertiesData() {
        List<Dict> dictList = dictService.selectDict(Dict.builder().dictType(DictTypeEnum.SYSTEM_PROPERTY.getType()).build());
        Optional.ofNullable(dictList).orElse(Collections.emptyList()).forEach(dict -> {
            if (StringUtils.isNotBlank(dict.getDictKey())) {
                try {
                    System.setProperty(dict.getDictKey(), dict.getDictValue());
                    distributedCacheComponent.put(dict.getDictKey(), dict.getDictValue());
                    log.info("Init System Properties Data key [{}] value [{}]", dict.getDictKey(), dict.getDictValue());
                } catch (Exception ex) {
                    log.error("Init System Properties Data key [{}] value [{}] error [{}]", dict.getDictKey(), dict.getDictValue(), ExceptionUtils.getStackTrace(ex));
                }
            }
        });
    }

    @Override
    public void onApplicationEvent(WebServerInitializedEvent event) {
        WebServer webServer = event.getWebServer();
        if (webServer instanceof UndertowWebServer undertowWebServer) {

            // 通过反射获取底层的 XnioWorker（Spring Boot 封装了直接访问方法）
            //this.xnioWorker = extractXnioWorker(undertowWebServer);
        }
    }

    //@Override
    //public void onApplicationEvent(WebServerInitializedEvent event) {
    //    WebServer webServer = event.getWebServer();
    //    if (webServer instanceof JettyWebServer) {
    //        JettyWebServer jettyWebServer = (JettyWebServer) webServer;
    //        this.jettyServer = jettyWebServer.getServer();
    //    }
    //}
    //
    //private void jetty() {
    //    ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor();
    //    executor.scheduleAtFixedRate(() -> {
    //        if (jettyServer.getThreadPool() instanceof QueuedThreadPool) {
    //            QueuedThreadPool threadPool = (QueuedThreadPool) jettyServer.getThreadPool();
    //            int max = threadPool.getMaxThreads();
    //            int busy = threadPool.getBusyThreads();
    //            int queue = threadPool.getQueueSize();
    //            System.out.printf("[Jetty线程池监控] Max=%d, Busy=%d, Queue=%d\n", max, busy, queue);
    //        }
    //    }, 0, 10, TimeUnit.SECONDS);
    //}
}
