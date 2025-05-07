package com.veadan.folib.ws.common;

import cn.hutool.http.HttpUtil;
import com.veadan.folib.components.node.NodeComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.utils.UrlUtils;
import jakarta.websocket.Session;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Objects;

/**
 * @author pengYongQiang
 * @date 2024/2/24 13:53
 */
@Slf4j
public class FolibWsRunManageUtil {

    public static String getWsURI(ClusterDispatchNodeDto nodeInfo, ConfigurationManager configurationManager) {
        final String clusterNodeHost = nodeInfo.getClusterNodeHost();
        final URL destUrl;
        final URL originUrl;
        try {
            destUrl = new URL(clusterNodeHost);
            originUrl = new URL(configurationManager.getConfiguration().getBaseUrl());
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
        final String originHost = originUrl.getHost();
        final Integer originPort = UrlUtils.getPort(originUrl.toString());
        final String destHost = destUrl.getHost();
        final Integer destPort = UrlUtils.getPort(clusterNodeHost);
        final String originNodeName = String.format("%s:%s", originHost, originPort);
        final String destUri = String.format("/wsv2/folib/%s", originNodeName);
        final boolean enableSSL = HttpUtil.isHttps(clusterNodeHost);
        return String.format("%s://%s:%s_%s", enableSSL ? "wss" : "ws", destHost, destPort + destUri, getEndpoint());
    }


    public static String getTargetHostName(ClusterDispatchNodeDto nodeInfo) {
        String clusterNodeHost = nodeInfo.getClusterNodeHost();
        return getTargetHostName(clusterNodeHost);
    }

    public static String getTargetHostName(String clusterNodeHost) {
        URL destUrl = null;
        try {
            destUrl = new URL(clusterNodeHost);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
        String destHost = destUrl.getHost();
        Integer destPort = UrlUtils.getPort(clusterNodeHost);
        return String.format("%s:%s_%s", destHost, destPort, getEndpoint());
    }

    public static String getTargetNode(String clusterNodeHost) {
        String targetNode = getTargetHostName(clusterNodeHost);
        FolibWsRunManageV2 wsRunManageV2 = SpringContextUtil.getBean(FolibWsRunManageV2.class);
        targetNode = wsRunManageV2.getTargetNode(targetNode);
        if (StringUtils.isBlank(targetNode)) {
            return "";
//            throw new RuntimeException(String.format("Target node [%s] not found ws session, please check target node status", getSimpleTargetHostName(clusterNodeHost)));
        }
        Session session = wsRunManageV2.getSession(targetNode, true);
        if (Objects.isNull(session) || !session.isOpen()) {
            return getTargetNode(clusterNodeHost);
        }
        log.info("Find WS targetNode [{}]", targetNode);
        return targetNode;
    }

    public static String getSimpleTargetHostName(ClusterDispatchNodeDto nodeInfo) {
        String clusterNodeHost = nodeInfo.getClusterNodeHost();
        return getSimpleTargetHostName(clusterNodeHost);
    }

    public static String getSimpleTargetHostName(String clusterNodeHost) {
        URL destUrl = null;
        try {
            destUrl = new URL(clusterNodeHost);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
        String destHost = destUrl.getHost();
        Integer destPort = UrlUtils.getPort(clusterNodeHost);
        return String.format("%s:%s", destHost, destPort);
    }

    public static String getEndpoint() {
        NodeComponent nodeComponent = SpringContextUtil.getBean(NodeComponent.class);
        return nodeComponent.cassandraClusterInfo().getEndpoint();
    }

    public static String resolverTargetHostName(String targetHostName) {
        if (targetHostName.contains("_")) {
            targetHostName = targetHostName.split("_")[0];
        }
        return targetHostName;
    }

}
