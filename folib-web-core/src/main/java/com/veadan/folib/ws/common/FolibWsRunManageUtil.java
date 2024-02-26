package com.veadan.folib.ws.common;

import cn.hutool.http.HttpUtil;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.utils.UrlUtils;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * @author pengYongQiang
 * @date 2024/2/24 13:53
 */
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
        return String.format("%s://%s:%s", enableSSL ? "wss" : "ws", destHost, destPort + destUri);
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
        return String.format("%s:%s", destHost, destPort);
    }

}
