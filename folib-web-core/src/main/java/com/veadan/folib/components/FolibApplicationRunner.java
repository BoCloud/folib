package com.veadan.folib.components;

import com.veadan.folib.config.janusgraph.JanusGraphDbProfile;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.enums.UpgradeTaskStatusEnum;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.scanner.service.ScanService;
import com.veadan.folib.services.DictService;
import com.veadan.folib.services.NodeService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * @author leipenghui
 * @date 2023/3/24
 **/
@Slf4j
@Component
public class FolibApplicationRunner implements ApplicationRunner {

    @Autowired
    private ScanService scanService;

    @Autowired
    private NodeService nodeService;

    @Autowired
    private DictService dictService;

    @Override
    public void run(ApplicationArguments args) throws Exception {
        this.initData();
    }

    /**
     * 初始化数据
     */
    private void initData() {
        initSystemPropertiesData();
        int total = scanService.countProperties();
        boolean isFirst = total <= 1;
        log.info("Table properties data total is {} ", total);
        if (isFirst) {
            if (JanusGraphDbProfile.PROFILE_EMBEDDED.equals(System.getProperty(JanusGraphDbProfile.PROPERTY_PROFILE))) {
                String clusterNodeTotal = System.getProperty("CLUSTER_NODE_TOTAL");
                if (StringUtils.isNotBlank(clusterNodeTotal)) {
                    log.info("Modify the cassandra replication factor ：{} ", clusterNodeTotal);
                    nodeService.modifyReplicationFactor(Integer.parseInt(clusterNodeTotal));
                }
            }
            log.info("The initialization of vulnerability data begins ");
            scanService.updateMirror();
            log.info("The initialization of vulnerability data ends ");
        }
        handlerUnExecutedTask();
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
                if (methodKey.equals(dict.getDictKey())) {
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
                                targetMethod.invoke(proxyObject, dict.getAlias());
                            } else {
                                targetMethod.invoke(proxyObject);
                            }
                            log.info("执行升级task：{} {}", clazz, methodName);
                            dict.setComment(UpgradeTaskStatusEnum.EXECUTED_SUCCESS.getStatus());
                            dictService.updateUnExecutedTask(dict);
                        }
                    } catch (Exception ex) {
                        dict.setComment(UpgradeTaskStatusEnum.EXECUTED_FAIL.getStatus());
                        dictService.updateUnExecutedTask(dict);
                        log.error("执行升级task错误：{}", ExceptionUtils.getStackTrace(ex));
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
                System.setProperty(dict.getDictKey(), dict.getDictValue());
                log.info("Init System Properties Data key：{}, value：{}", dict.getDictKey(), dict.getDictValue());
            }
        });
    }

}
