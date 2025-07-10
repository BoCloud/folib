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
package com.folib.licence;


import com.folib.components.node.NodeComponent;
import com.folib.scanner.common.util.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

/**
 * @author veadan
 **/
@Slf4j
public class MacUtil {

    public static void main(String[] args) throws Exception {
        System.out.println("getMachineCode = " + getMachineCode());
    }

    /**
     * 获取机器码
     *
     * @return 机器码
     * @throws Exception 异常信息
     */
    public static String getMachineCode() throws Exception {
        Set<String> result = new HashSet<>();
        NodeComponent nodeComponent = SpringContextUtil.getBean(NodeComponent.class);
        String localHostId = nodeComponent.cassandraClusterInfo().getLocalHostId();
        log.debug("MachineCode localHostId [{}]", localHostId);
        result.add(localHostId);
        Properties props = System.getProperties();
        String javaVersion = props.getProperty("java.version");
        result.add(javaVersion);
        String javaVMVersion = props.getProperty("java.vm.version");
        result.add(javaVMVersion);
        String osVersion = props.getProperty("os.version");
        result.add(osVersion);
        String code = Encrpt.GetMD5Code(result.toString());
        return getSplitString(code, "-", 4);
    }


    /**
     * 分隔字符串
     *
     * @param str    源字符串
     * @param split  分隔字符
     * @param length 分隔长度
     * @return 分隔后的字符串
     */
    public static String getSplitString(String str, String split, int length) {
        int len = str.length();
        StringBuilder temp = new StringBuilder();
        for (int i = 0; i < len; i++) {
            if (i % length == 0 && i > 0) {
                temp.append(split);
            }
            temp.append(str.charAt(i));
        }
        String[] attrs = temp.toString().split(split);
        StringBuilder finalMachineCode = new StringBuilder();
        for (String attr : attrs) {
            if (attr.length() == length) {
                finalMachineCode.append(attr).append(split);
            }
        }
        return finalMachineCode.toString().substring(0,
                finalMachineCode.toString().length() - 1);
    }

}
