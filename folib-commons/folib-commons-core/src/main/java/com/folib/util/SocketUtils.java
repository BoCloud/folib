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
package com.folib.util;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;

import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;

/**
 * @author veadan
 * @date 2024/1/10
 **/
@Slf4j
public class SocketUtils {

    /**
     * 判断某服务能否连通
     *
     * @param host host
     * @param port port
     * @return boolean
     */
    public static boolean isRunning(String host, int port) {
        Socket sClient = null;
        try {
            SocketAddress saAdd = new InetSocketAddress(host.trim(), port);
            sClient = new Socket();
            sClient.connect(saAdd, 1000);
        } catch (Exception e) {
            log.warn("Host [{}] port [{}] error [{}]", host, port, ExceptionUtils.getStackTrace(e));
            return false;
        } finally {
            try {
                if (sClient != null) {
                    sClient.close();
                }
            } catch (Exception e) {
            }
        }
        return true;
    }
}
